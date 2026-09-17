/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Loads OpenAPI/AsyncAPI specifications from Maven dependencies.
 * Supports explicit Maven artifact specification to eliminate classpath ambiguity.
 *
 * v7.1+ feature: Load specs from specific Maven dependencies instead of classpath.
 */
public class DependencySpecLoader {

  private static final Logger LOGGER = LoggerFactory.getLogger(DependencySpecLoader.class);
  private static final Map<String, URLClassLoader> LOADER_CACHE = new ConcurrentHashMap<>();
  private static final ObjectMapper YAML_MAPPER = new ObjectMapper(new YAMLFactory());
  private static final ObjectMapper JSON_MAPPER = new ObjectMapper();

  private DependencySpecLoader() {
    // Utility class
  }

  /**
   * Loads a specification from a specific Maven dependency.
   *
   * @param filePath path to spec file within the JAR (e.g., "specs/api.yml")
   * @param groupId Maven groupId (e.g., "com.company")
   * @param artifactId Maven artifactId (e.g., "api-spec-consumidor")
   * @param version Maven version or null to use from pom.xml
   * @return Parsed JSON representation of the spec
   * @throws IOException if JAR cannot be resolved or spec not found
   */
  public static JsonNode loadSpec(
      final String filePath,
      final String groupId,
      final String artifactId,
      final String version) throws IOException {

    if (StringUtils.isBlank(filePath) || StringUtils.isBlank(groupId) || StringUtils.isBlank(artifactId)) {
      throw new IllegalArgumentException("filePath, groupId, and artifactId are required");
    }

    // Step 1: Resolve JAR from Maven repository
    final File jarFile = resolveMavenArtifact(groupId, artifactId, version);
    if (!jarFile.exists()) {
      throw new IOException(String.format(
          "Cannot resolve Maven artifact: %s:%s:%s (not found at %s)",
          groupId, artifactId, StringUtils.defaultIfBlank(version, "default"), jarFile.getAbsolutePath()));
    }

    LOGGER.debug("Resolved Maven artifact: {} -> {}", String.format("%s:%s:%s", groupId, artifactId, version), jarFile);

    // Step 2: Get or create isolated URLClassLoader for this JAR
    final String cacheKey = String.format("%s:%s:%s", groupId, artifactId, StringUtils.defaultIfBlank(version, "latest"));
    final URLClassLoader jarLoader = LOADER_CACHE.computeIfAbsent(cacheKey, k -> {
      try {
        return new URLClassLoader(new URL[]{jarFile.toURI().toURL()}, null);
      } catch (final MalformedURLException e) {
        throw new RuntimeException("Failed to create URLClassLoader for JAR: " + jarFile, e);
      }
    });

    LOGGER.debug("Loaded spec from JAR: {} (path: {})", jarFile.getName(), filePath);

    // Step 3: Load and parse spec from JAR
    return loadSpecFromJar(filePath, jarLoader);
  }

  /**
   * Loads a specification file from an already-resolved JAR loader.
   *
   * @param filePath path to spec file within the JAR
   * @param jarLoader URLClassLoader pointing to the spec JAR
   * @return Parsed JSON representation of the spec
   * @throws IOException if spec not found or parsing fails
   */
  public static JsonNode loadSpecFromJar(final String filePath, final URLClassLoader jarLoader) throws IOException {
    Objects.requireNonNull(jarLoader, "jarLoader cannot be null");

    try (InputStream spec = jarLoader.getResourceAsStream(filePath)) {
      if (spec == null) {
        throw new IOException(String.format("Spec not found in JAR: %s (path: %s)", jarLoader, filePath));
      }

      // Parse YAML or JSON based on file extension
      if (filePath.endsWith(".yml") || filePath.endsWith(".yaml")) {
        return YAML_MAPPER.readTree(spec);
      } else if (filePath.endsWith(".json")) {
        return JSON_MAPPER.readTree(spec);
      } else {
        // Try YAML first, then JSON
        try {
          return YAML_MAPPER.readTree(spec);
        } catch (final Exception yamlException) {
          // Reset stream and try JSON
          spec.reset();
          return JSON_MAPPER.readTree(spec);
        }
      }
    }
  }

  /**
   * Resolves a Maven artifact to a JAR file in the local M2 repository.
   *
   * @param groupId Maven groupId
   * @param artifactId Maven artifactId
   * @param version Maven version (can be null for "latest")
   * @return File pointing to the JAR in ~/.m2/repository
   */
  private static File resolveMavenArtifact(final String groupId, final String artifactId, final String version) {
    final String m2Repo = System.getProperty("user.home") + "/.m2/repository";
    final String groupPath = groupId.replace(".", File.separator);
    final String versionDir = StringUtils.isNotBlank(version) ? version : "LATEST";

    final String path = String.format("%s%s%s%s%s%s%s-%s.jar",
        m2Repo,
        File.separator,
        groupPath,
        File.separator,
        artifactId,
        File.separator,
        versionDir,
        File.separator,
        artifactId,
        versionDir);

    return new File(path);
  }

  /**
   * Clears the JAR loader cache. Useful for testing or cleanup.
   */
  public static void clearCache() {
    LOGGER.debug("Clearing DependencySpecLoader cache");
    LOADER_CACHE.values().forEach(loader -> {
      try {
        loader.close();
      } catch (final IOException e) {
        LOGGER.warn("Failed to close URLClassLoader", e);
      }
    });
    LOADER_CACHE.clear();
  }
}
