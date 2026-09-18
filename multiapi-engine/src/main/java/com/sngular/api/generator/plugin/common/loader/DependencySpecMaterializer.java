/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.common.model.ExternalSpecSource;
import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

/**
 * Makes a contract published inside an artifact readable as an ordinary file.
 *
 * <p>The artifact is resolved through a {@link SpecArtifactResolver} and unpacked once per build
 * under the module's build directory; the spec's {@code filePath} is then rewritten to the
 * extracted copy. Everything downstream — relative {@code $ref} resolution, base URI computation,
 * the model builder — keeps working on plain files, which is what makes multi-file contracts inside
 * an artifact resolve correctly instead of only the root document.</p>
 */
@Slf4j
public class DependencySpecMaterializer {

  private static final String EXTRACTION_FOLDER = "multiapi-specs";

  private static final String MARKER_FILE = ".multiapi-extracted";

  private static final int MAX_LISTED_CANDIDATES = 20;

  /** Parses YAML and JSON alike, and is only used to tell a root contract from a fragment. */
  private static final ObjectMapper SPEC_MAPPER = new ObjectMapper(new YAMLFactory());

  private final SpecArtifactResolver artifactResolver;

  private final Path extractionRoot;

  private final Map<String, Path> extractedArtifacts = new ConcurrentHashMap<>();

  public DependencySpecMaterializer(final SpecArtifactResolver artifactResolver, final File targetFolder) {
    this.artifactResolver = artifactResolver;
    this.extractionRoot = targetFolder.toPath().resolve("generated-resources").resolve(EXTRACTION_FOLDER);
  }

  /**
   * Resolves the artifact declared by {@code specSource} and returns the absolute path of the
   * contract extracted from it.
   *
   * @param specSource a spec configuration whose {@code usesExternalDependency()} is {@code true}.
   * @param rootMarker the top-level field that marks a root document of the kind being generated,
   *                   {@code "openapi"} or {@code "asyncapi"}. Used only to default a missing
   *                   {@code filePath}.
   * @return the absolute path of the extracted contract.
   */
  public Path materialize(final ExternalSpecSource specSource, final String rootMarker) {
    final String groupId = specSource.getFromGroupId();
    final String artifactId = specSource.getFromArtifactId();
    final String version = specSource.getFromVersion();
    final String filePath = specSource.getFilePath();

    final File artifact = artifactResolver.resolveArtifact(groupId, artifactId, version);
    final Path artifactContent = extractedArtifacts.computeIfAbsent(artifact.getAbsolutePath(), key -> extract(artifact, groupId, artifactId));

    if (StringUtils.isBlank(filePath)) {
      return theOnlyContractIn(artifactContent, specSource, artifact, rootMarker);
    }

    final Path specPath = resolveInsideArtifact(artifactContent, filePath);
    if (!Files.isRegularFile(specPath)) {
      throw new SpecDependencyException(String.format(
          "Spec '%s' not found inside %s.%s", filePath, specSource.getDependencyCoordinate(), describeCandidates(artifactContent)));
    }

    log.info("Loading spec '{}' from dependency {} ({})", filePath, specSource.getDependencyCoordinate(), artifact);
    return specPath;
  }

  /**
   * An artifact that publishes a single contract does not need its path repeated in every consumer,
   * so {@code filePath} may be omitted there.
   *
   * <p>Counting spec <em>files</em> would not do: a multi-file contract ships its schema fragments
   * beside the root document, and they are {@code .yml} files too. Only documents carrying the
   * {@code openapi} or {@code asyncapi} top-level field are root contracts, which also keeps an
   * artifact that publishes both kinds from feeding the wrong one to the wrong generator. With
   * several root contracts it stays required, because choosing one would be a guess at which API to
   * generate.</p>
   */
  private static Path theOnlyContractIn(
      final Path artifactContent, final ExternalSpecSource specSource, final File artifact, final String rootMarker) {

    final List<Path> contracts = specCandidates(artifactContent).stream()
                                                                .filter(candidate -> isRootContract(candidate, rootMarker))
                                                                .toList();

    if (contracts.isEmpty()) {
      throw new SpecDependencyException(String.format(
          "No %s contract found inside %s: no file declares a top-level '%s' field.%s",
          rootMarker, specSource.getDependencyCoordinate(), rootMarker, describeCandidates(artifactContent)));
    }
    if (contracts.size() > 1) {
      throw new SpecDependencyException(String.format(
          "filePath is required for %s: the artifact carries %d %s contracts.%s",
          specSource.getDependencyCoordinate(), contracts.size(), rootMarker, describe(artifactContent, contracts)));
    }

    final Path specPath = contracts.get(0);
    log.info("Loading spec '{}' from dependency {} ({}), the only {} contract it carries",
        toEntryName(artifactContent.relativize(specPath)), specSource.getDependencyCoordinate(), artifact, rootMarker);
    return specPath;
  }

  /**
   * @return {@code true} when the file is a root document of the requested kind rather than a
   *     schema fragment referenced by one. An unreadable or unparseable file is not one.
   */
  private static boolean isRootContract(final Path candidate, final String rootMarker) {
    try {
      return SPEC_MAPPER.readTree(candidate.toFile()).hasNonNull(rootMarker);
    } catch (final IOException | RuntimeException e) {
      log.debug("Could not read {} while looking for a root contract", candidate, e);
      return false;
    }
  }

  /**
   * Resolves a path declared inside the artifact, rejecting anything that would escape the
   * extracted content.
   */
  private static Path resolveInsideArtifact(final Path artifactContent, final String filePath) {
    final String normalized = StringUtils.removeStart(StringUtils.removeStart(filePath.replace('\\', '/'), "./"), "/");
    final Path resolved = artifactContent.resolve(normalized).normalize();
    if (!resolved.startsWith(artifactContent)) {
      throw new SpecDependencyException(String.format("filePath '%s' points outside the artifact content.", filePath));
    }
    return resolved;
  }

  private Path extract(final File artifact, final String groupId, final String artifactId) {
    final String artifactFolder = StringUtils.removeEnd(artifact.getName(), ".jar");
    final Path destination = extractionRoot.resolve(groupId).resolve(artifactId).resolve(artifactFolder);
    final Path marker = destination.resolve(MARKER_FILE);
    final String stamp = artifact.getAbsolutePath() + "@" + artifact.lastModified();

    try {
      if (Files.isRegularFile(marker) && stamp.equals(Files.readString(marker, StandardCharsets.UTF_8))) {
        log.debug("Reusing already extracted artifact {}", destination);
        return destination;
      }
      Files.createDirectories(destination);
      unzip(artifact, destination);
      Files.writeString(marker, stamp, StandardCharsets.UTF_8);
    } catch (final IOException e) {
      throw new SpecDependencyException(String.format("Could not unpack %s into %s.", artifact, destination), e);
    }
    return destination;
  }

  private static void unzip(final File artifact, final Path destination) throws IOException {
    try (ZipFile zipFile = new ZipFile(artifact)) {
      for (final var entries = zipFile.entries(); entries.hasMoreElements(); ) {
        final ZipEntry entry = entries.nextElement();
        if (entry.isDirectory() || entry.getName().endsWith(".class")) {
          continue;
        }
        final Path target = destination.resolve(entry.getName()).normalize();
        // Zip slip: an entry name such as "../../evil" must never write outside the destination.
        if (!target.startsWith(destination)) {
          throw new IOException("Entry '" + entry.getName() + "' would be written outside " + destination);
        }
        Files.createDirectories(target.getParent());
        try (InputStream content = zipFile.getInputStream(entry)) {
          Files.copy(content, target, StandardCopyOption.REPLACE_EXISTING);
        }
      }
    }
  }

  /**
   * A missing spec is almost always a wrong path inside the artifact, so the error lists what the
   * artifact actually carries instead of leaving the user to unzip it by hand.
   */
  private static String describeCandidates(final Path artifactContent) {
    return describe(artifactContent, specCandidates(artifactContent));
  }

  private static String describe(final Path artifactContent, final List<Path> paths) {
    final List<String> names = paths.stream()
                                    .limit(MAX_LISTED_CANDIDATES)
                                    .map(artifactContent::relativize)
                                    .map(DependencySpecMaterializer::toEntryName)
                                    .toList();
    return names.isEmpty()
        ? " The artifact contains no .yml, .yaml or .json file."
        : names.stream().collect(Collectors.joining("\n  - ", " Available specs:\n  - ", ""));
  }

  /**
   * The spec files an artifact carries, in a stable order. Packaging metadata is skipped so that a
   * {@code META-INF} descriptor never counts as a contract.
   */
  private static List<Path> specCandidates(final Path artifactContent) {
    try (var paths = Files.walk(artifactContent)) {
      return paths.filter(Files::isRegularFile)
                  .filter(path -> isSpecCandidate(toEntryName(artifactContent.relativize(path))))
                  .sorted()
                  .toList();
    } catch (final IOException e) {
      log.debug("Could not list the artifact content at {}", artifactContent, e);
      return List.of();
    }
  }

  private static String toEntryName(final Path relativePath) {
    return relativePath.toString().replace('\\', '/');
  }

  private static boolean isSpecCandidate(final String entryName) {
    return StringUtils.endsWithAny(entryName, ".yml", ".yaml", ".json") && !entryName.startsWith("META-INF/");
  }
}
