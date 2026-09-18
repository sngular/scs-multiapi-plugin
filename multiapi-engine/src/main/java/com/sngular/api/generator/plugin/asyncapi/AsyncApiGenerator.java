/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.sngular.api.generator.plugin.asyncapi.handler.AsyncApiHandlerFactory;
import com.sngular.api.generator.plugin.asyncapi.handler.BaseAsyncApiHandler;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.common.files.ClasspathFileLocation;
import com.sngular.api.generator.plugin.common.loader.DependencySpecMaterializer;
import com.sngular.api.generator.plugin.common.loader.LocalRepositorySpecArtifactResolver;
import com.sngular.api.generator.plugin.common.loader.SpecArtifactResolver;
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocationUtil;
import com.sngular.api.generator.plugin.common.files.RemoteFileLocation;
import com.sngular.api.generator.plugin.common.tools.PathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

@Slf4j
public class AsyncApiGenerator {

  private final Integer springBootVersion;

  private final boolean overwriteModel;

  private final File targetFolder;

  private final String processedGeneratedSourcesFolder;

  private final String groupId;

  private final File baseDir;

  private SpecArtifactResolver artifactResolver = new LocalRepositorySpecArtifactResolver();

  private DependencySpecMaterializer specMaterializer;

  public AsyncApiGenerator(
      final Integer springBootVersion,
      boolean overwriteModel,
      final File targetFolder,
      final String processedGeneratedSourcesFolder,
      final String groupId,
      final File baseDir) {
    log.debug("Initializing AsyncApiGenerator with Spring Boot version:{}", springBootVersion);
    this.springBootVersion = springBootVersion;
    this.overwriteModel = overwriteModel;
    this.targetFolder = targetFolder;
    this.processedGeneratedSourcesFolder = processedGeneratedSourcesFolder;
    this.groupId = groupId;
    this.baseDir = baseDir;
  }

  /**
   * Installs the resolver used to fetch artifacts declared through {@code fromGroupId}/
   * {@code fromArtifactId}. Build-tool plugins supply their own so that private repositories,
   * mirrors and credentials configured for the build are honoured; without it only the local
   * repository is inspected.
   */
  public final void setArtifactResolver(final SpecArtifactResolver artifactResolver) {
    this.artifactResolver = Objects.requireNonNull(artifactResolver, "artifactResolver");
    this.specMaterializer = null;
  }

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    log.info("Processing {} spec files", specsListFile.size());

    // Process each spec file with its appropriate handler
    for (SpecFile specFile : specsListFile) {
      final SpecFile resolvedSpecFile = resolveSpecFile(specFile);
      try {
        final String filePath = resolvedSpecFile.getFilePath();
        final Pair<InputStream, FileLocation> ymlLocation = resolveYmlLocation(filePath);
        final ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
        final JsonNode openApi = mapper.readTree(ymlLocation.getKey());
        final String version = getAsyncApiVersion(openApi);
        BaseAsyncApiHandler handler = AsyncApiHandlerFactory
                                          .getHandler(version, springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
        handler.processFileSpec(Collections.singletonList(resolvedSpecFile));
      } catch (IOException e) {
        log.error("Error processing spec file: {}", specFile.getFilePath(), e);
        // Continue with next file
      }
    }
  }

  /**
   * Replaces the configured {@code filePath} with the contract extracted from the declared
   * artifact, so the rest of the pipeline reads an ordinary file. Specs without dependency
   * coordinates are returned untouched.
   */
  private SpecFile resolveSpecFile(final SpecFile specFile) {
    specFile.validateDependencyCoordinates();
    if (!specFile.usesExternalDependency()) {
      return specFile;
    }
    if (Objects.isNull(specMaterializer)) {
      specMaterializer = new DependencySpecMaterializer(artifactResolver, targetFolder);
    }
    return specFile.toBuilder().filePath(specMaterializer.materialize(specFile, "asyncapi").toString()).build();
  }

  private static Pair<InputStream, FileLocation> resolveYmlLocation(final String ymlFilePath) throws FileNotFoundException {
    log.debug("Resolving YAML file location:{}", ymlFilePath);

    // Remote URIs: fetch directly from URL
    if (PathUtil.isRemoteUri(ymlFilePath)) {
      log.debug("Loading spec from remote URL");
      try {
        final URI uri = URI.create(ymlFilePath);
        final InputStream remoteStream = PathUtil.openUrlStream(uri.toURL());
        return new ImmutablePair<>(remoteStream, new RemoteFileLocation(uri.resolve(".")));
      } catch (final IOException e) {
        throw new FileNotFoundException("Could not open remote YAML file: " + ymlFilePath);
      }
    }

    // Classpath resources: use getResource() to get actual location (including JAR path)
    try {
      final var classPathResource = AsyncApiGenerator.class.getClassLoader().getResource(ymlFilePath);
      if (Objects.nonNull(classPathResource)) {
        log.debug("Found file in classpath: {}", classPathResource);
        final URI resourceUri = classPathResource.toURI();
        final InputStream ymlFile = classPathResource.openStream();
        final URI parentUri = FileLocationUtil.getParentUri(resourceUri);
        final FileLocation ymlParentPath = new ClasspathFileLocation(parentUri);
        return new ImmutablePair<>(ymlFile, ymlParentPath);
      }
    } catch (final Exception e) {
      log.debug("Classpath resolution failed, trying filesystem: {}", e.getMessage());
    }

    // Filesystem fallback
    log.debug("Looking for file in filesystem");
    final File f = new File(ymlFilePath);
    if (!f.exists()) {
      throw new FileNotFoundException("Could not find YAML file: " + ymlFilePath);
    }

    final InputStream ymlFile = new FileInputStream(f);
    final FileLocation ymlParentPath;
    if (PathUtil.isAbsolutePath(ymlFilePath)) {
      ymlParentPath = new DirectoryFileLocation(Paths.get(ymlFilePath).getParent());
    } else {
      ymlParentPath = new DirectoryFileLocation(f.toPath().getParent());
    }
    return new ImmutablePair<>(ymlFile, ymlParentPath);
  }

  private static String getAsyncApiVersion(final JsonNode openApi) {
    if (openApi.has("asyncapi")) {
      return openApi.get("asyncapi").asText();
    }
    log.warn("No AsyncAPI version specified, defaulting to 2.0.0");
    return "2.0.0"; // Default to 2.0.0 if version is not specified
  }
}
