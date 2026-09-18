/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import com.sngular.api.generator.plugin.openapi.parameter.SpecFile;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("Materializing a spec published inside an artifact")
class DependencySpecMaterializerTest {

  private static final String GROUP_ID = "com.company";

  private static final String ARTIFACT_ID = "api-specs";

  private static final String VERSION = "1.0.0";

  private static final String SPEC_PATH = "specs/openapi.yml";

  private static final String SPEC_CONTENT = "openapi: 3.0.3";

  private static final String FRAGMENT = "type: object";

  @TempDir
  Path tempDir;

  @Test
  @DisplayName("extracts the spec declared inside the artifact and returns its path")
  void materializesSpecFromArtifact() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT));

    final Path materialized = materializer(artifact).materialize(specFile(SPEC_PATH, VERSION), "openapi");

    assertThat(materialized).isRegularFile().hasContent(SPEC_CONTENT);
  }

  @Test
  @DisplayName("extracts the whole artifact so relative refs inside it resolve as ordinary files")
  void materializesReferencedFilesAlongsideTheSpec() throws IOException {
    final File artifact = artifactContaining(new LinkedHashMap<>(Map.of(
        SPEC_PATH, SPEC_CONTENT,
        "specs/schemas/user.yml", FRAGMENT)));

    final Path materialized = materializer(artifact).materialize(specFile(SPEC_PATH, VERSION), "openapi");

    assertThat(materialized.getParent().resolve("schemas/user.yml")).isRegularFile().hasContent(FRAGMENT);
  }

  @Test
  @DisplayName("unpacks an artifact only once, even across several specs")
  void reusesTheExtractedArtifact() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT));
    final DependencySpecMaterializer materializer = materializer(artifact);

    final Path first = materializer.materialize(specFile(SPEC_PATH, VERSION), "openapi");
    Files.writeString(first, "touched by the build", StandardCharsets.UTF_8);
    final Path second = materializer.materialize(specFile(SPEC_PATH, VERSION), "openapi");

    assertThat(second).isEqualTo(first).hasContent("touched by the build");
  }

  @Test
  @DisplayName("re-unpacks when the resolved artifact changed")
  void reExtractsWhenTheArtifactChanged() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT));
    final Path materialized = materializer(artifact).materialize(specFile(SPEC_PATH, VERSION), "openapi");
    Files.writeString(materialized, "stale", StandardCharsets.UTF_8);

    writeArtifact(artifact, Map.of(SPEC_PATH, "openapi: 3.1.0"));
    assertThat(artifact.setLastModified(System.currentTimeMillis() + 1000L)).isTrue();

    assertThat(materializer(artifact).materialize(specFile(SPEC_PATH, VERSION), "openapi")).hasContent("openapi: 3.1.0");
  }

  @Test
  @DisplayName("defaults filePath to the only contract the artifact carries")
  void defaultsToTheOnlyContractInTheArtifact() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT, "META-INF/MANIFEST.MF", "Manifest-Version: 1.0"));

    final Path materialized = materializer(artifact).materialize(specFile(null, VERSION), "openapi");

    assertThat(materialized).isRegularFile().hasContent(SPEC_CONTENT);
  }

  @Test
  @DisplayName("ignores the schema fragments of a multi-file contract when defaulting filePath")
  void defaultsPastTheFragmentsOfAMultiFileContract() throws IOException {
    final File artifact = artifactContaining(new LinkedHashMap<>(Map.of(
        SPEC_PATH, SPEC_CONTENT,
        "specs/schemas/user.yml", FRAGMENT,
        "specs/schemas/common.yml", FRAGMENT)));

    final Path materialized = materializer(artifact).materialize(specFile(null, VERSION), "openapi");

    assertThat(materialized).isRegularFile().hasContent(SPEC_CONTENT);
  }

  @Test
  @DisplayName("defaults to the contract of the kind being generated, not the other one")
  void defaultsToTheContractOfTheRequestedKind() throws IOException {
    final File artifact = artifactContaining(new LinkedHashMap<>(Map.of(
        SPEC_PATH, SPEC_CONTENT,
        "specs/events.yml", "asyncapi: 2.6.0")));

    assertThat(materializer(artifact).materialize(specFile(null, VERSION), "asyncapi")).hasContent("asyncapi: 2.6.0");
    assertThat(materializer(artifact).materialize(specFile(null, VERSION), "openapi")).hasContent(SPEC_CONTENT);
  }

  @Test
  @DisplayName("asks for filePath when the artifact carries more than one contract")
  void requiresFilePathWhenTheArtifactCarriesSeveralContracts() throws IOException {
    final File artifact = artifactContaining(new LinkedHashMap<>(Map.of(
        SPEC_PATH, SPEC_CONTENT,
        "specs/other.yml", "openapi: 3.1.0")));

    assertThatThrownBy(() -> materializer(artifact).materialize(specFile(null, VERSION), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("filePath is required for com.company:api-specs:1.0.0")
        .hasMessageContaining("carries 2 openapi contracts")
        .hasMessageContaining(SPEC_PATH)
        .hasMessageContaining("specs/other.yml");
  }

  @Test
  @DisplayName("says so when the artifact carries no contract of that kind")
  void reportsAnArtifactWithoutContracts() throws IOException {
    final File artifact = artifactContaining(Map.of("specs/schemas/user.yml", FRAGMENT));

    assertThatThrownBy(() -> materializer(artifact).materialize(specFile(null, VERSION), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("No openapi contract found inside com.company:api-specs:1.0.0")
        .hasMessageContaining("specs/schemas/user.yml");
  }

  @Test
  @DisplayName("lists the specs the artifact does carry when the path is wrong")
  void reportsAvailableSpecsWhenThePathIsWrong() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT));

    assertThatThrownBy(() -> materializer(artifact).materialize(specFile("openapi/openapi.yml", VERSION), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("openapi/openapi.yml")
        .hasMessageContaining("com.company:api-specs:1.0.0")
        .hasMessageContaining(SPEC_PATH);
  }

  @Test
  @DisplayName("refuses a filePath that escapes the artifact content")
  void refusesPathTraversal() throws IOException {
    final File artifact = artifactContaining(Map.of(SPEC_PATH, SPEC_CONTENT));

    assertThatThrownBy(() -> materializer(artifact).materialize(specFile("../../../etc/passwd", VERSION), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("points outside the artifact");
  }

  @Test
  @DisplayName("refuses an artifact entry that would be written outside the extraction folder")
  void refusesZipSlipEntries() throws IOException {
    final File artifact = artifactContaining(Map.of("../escaped.yml", SPEC_CONTENT));

    assertThatThrownBy(() -> materializer(artifact).materialize(specFile(SPEC_PATH, VERSION), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("Could not unpack");
  }

  @Test
  @DisplayName("rejects half-declared coordinates instead of silently reading from the filesystem")
  void rejectsIncompleteCoordinates() {
    final SpecFile specFile = SpecFile.builder().filePath(SPEC_PATH).fromArtifactId(ARTIFACT_ID).build();

    assertThat(specFile.usesExternalDependency()).isFalse();
    assertThatThrownBy(specFile::validateDependencyCoordinates)
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("fromGroupId and fromArtifactId must both be set");
  }

  @Test
  @DisplayName("looks the artifact up in the local repository when no build tool supplies a resolver")
  void fallsBackToTheLocalRepository() throws IOException {
    final Path localRepository = tempDir.resolve("m2");
    final Path artifactPath = localRepository.resolve("com/company/api-specs/1.0.0");
    Files.createDirectories(artifactPath);
    writeArtifact(artifactPath.resolve("api-specs-1.0.0.jar").toFile(), Map.of(SPEC_PATH, SPEC_CONTENT));

    final var materializer = new DependencySpecMaterializer(
        new LocalRepositorySpecArtifactResolver(localRepository), tempDir.resolve("target").toFile());

    assertThat(materializer.materialize(specFile(SPEC_PATH, VERSION), "openapi")).isRegularFile().hasContent(SPEC_CONTENT);
  }

  @Test
  @DisplayName("names the artifact it could not find in the local repository")
  void reportsAnArtifactMissingFromTheLocalRepository() {
    final var materializer = new DependencySpecMaterializer(
        new LocalRepositorySpecArtifactResolver(tempDir.resolve("m2")), tempDir.resolve("target").toFile());

    assertThatThrownBy(() -> materializer.materialize(specFile(SPEC_PATH, "9.9.9"), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("Cannot resolve com.company:api-specs:9.9.9");
  }

  @Test
  @DisplayName("asks for a version when neither the configuration nor the build provides one")
  void requiresAVersionInTheLocalRepositoryFallback() {
    final var materializer = new DependencySpecMaterializer(
        new LocalRepositorySpecArtifactResolver(tempDir.resolve("m2")), tempDir.resolve("target").toFile());

    assertThatThrownBy(() -> materializer.materialize(specFile(SPEC_PATH, null), "openapi"))
        .isInstanceOf(SpecDependencyException.class)
        .hasMessageContaining("No version available for com.company:api-specs");
  }

  private DependencySpecMaterializer materializer(final File artifact) {
    return new DependencySpecMaterializer(
        (groupId, artifactId, version) -> artifact, tempDir.resolve("target").toFile());
  }

  private static SpecFile specFile(final String filePath, final String version) {
    return SpecFile.builder()
                   .filePath(filePath)
                   .fromGroupId(GROUP_ID)
                   .fromArtifactId(ARTIFACT_ID)
                   .fromVersion(version)
                   .build();
  }

  private File artifactContaining(final Map<String, String> entries) throws IOException {
    final File artifact = tempDir.resolve("api-specs-1.0.0.jar").toFile();
    writeArtifact(artifact, entries);
    return artifact;
  }

  private static void writeArtifact(final File artifact, final Map<String, String> entries) throws IOException {
    try (OutputStream out = Files.newOutputStream(artifact.toPath());
        ZipOutputStream zip = new ZipOutputStream(out)) {
      for (final Map.Entry<String, String> entry : entries.entrySet()) {
        zip.putNextEntry(new ZipEntry(entry.getKey()));
        zip.write(entry.getValue().getBytes(StandardCharsets.UTF_8));
        zip.closeEntry();
      }
    }
  }
}
