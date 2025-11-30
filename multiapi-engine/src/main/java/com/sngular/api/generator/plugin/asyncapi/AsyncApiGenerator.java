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
import com.sngular.api.generator.plugin.common.files.DirectoryFileLocation;
import com.sngular.api.generator.plugin.common.files.FileLocation;
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

  public final void processFileSpec(final List<SpecFile> specsListFile) {
    log.info("Processing {} spec files", specsListFile.size());

    // Process each spec file with its appropriate handler
    for (SpecFile specFile : specsListFile) {
      try {
        final String filePath = specFile.getFilePath();
        final Pair<InputStream, FileLocation> ymlLocation = resolveYmlLocation(filePath);
        final ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
        final JsonNode openApi = mapper.readTree(ymlLocation.getKey());
        final String version = getAsyncApiVersion(openApi);
        BaseAsyncApiHandler handler = AsyncApiHandlerFactory
                                          .getHandler(version, springBootVersion, overwriteModel, targetFolder, processedGeneratedSourcesFolder, groupId, baseDir);
        handler.processFileSpec(Collections.singletonList(specFile));
      } catch (IOException e) {
        log.error("Error processing spec file: {}", specFile.getFilePath(), e);
        // Continue with next file
      }
    }
  }

  private static Pair<InputStream, FileLocation> resolveYmlLocation(final String ymlFilePath) throws FileNotFoundException {
    log.debug("Resolving YAML file location:{}", ymlFilePath);
    final InputStream classPathInput = AsyncApiGenerator.class.getClassLoader().getResourceAsStream(ymlFilePath);
    final InputStream ymlFile;
    final FileLocation ymlParentPath;
    if (Objects.nonNull(classPathInput)) {
      log.debug("Found file in classpath");
      ymlFile = classPathInput;
      ymlParentPath = new ClasspathFileLocation(URI.create(ymlFilePath));
    } else {
      log.debug("Looking for file in filesystem");
      final File f = new File(ymlFilePath);
      ymlFile = new FileInputStream(f);
      // For absolute paths, use the parent directly; otherwise, resolve relative to current directory
      if (PathUtil.isAbsolutePath(ymlFilePath)) {
        ymlParentPath = new DirectoryFileLocation(Paths.get(ymlFilePath).getParent());
      } else {
        ymlParentPath = new DirectoryFileLocation(f.toPath().getParent());
      }
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
