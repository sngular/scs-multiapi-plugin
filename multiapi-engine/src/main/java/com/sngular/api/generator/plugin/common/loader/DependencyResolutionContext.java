/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import java.io.IOException;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Manages JAR loaders and spec resolution for dependency-based spec loading.
 *
 * Maintains a mapping of Maven coordinates to URLClassLoaders, enabling
 * resolution of specs and refs within specific JARs.
 */
public class DependencyResolutionContext {

  private static final Logger LOGGER = LoggerFactory.getLogger(DependencyResolutionContext.class);

  private final Map<String, URLClassLoader> loadersByCoordinates = new HashMap<>();
  private final Map<String, JsonNode> specCache = new HashMap<>();

  /**
   * Loads a spec from the specified Maven dependency.
   *
   * @param filePath path to spec within JAR
   * @param groupId Maven groupId
   * @param artifactId Maven artifactId
   * @param version Maven version or null
   * @return Parsed spec JSON
   * @throws IOException if spec cannot be loaded
   */
  public JsonNode loadSpec(final String filePath, final String groupId, final String artifactId, final String version)
      throws IOException {

    final String cacheKey = String.format("%s:%s:%s:%s", groupId, artifactId, version, filePath);

    // Return cached spec if available
    if (specCache.containsKey(cacheKey)) {
      LOGGER.debug("Returning cached spec: {}", cacheKey);
      return specCache.get(cacheKey);
    }

    // Load spec from JAR and cache it
    final JsonNode spec = DependencySpecLoader.loadSpec(filePath, groupId, artifactId, version);
    specCache.put(cacheKey, spec);

    return spec;
  }

  /**
   * Gets or creates a URLClassLoader for the specified Maven dependency.
   *
   * @param groupId Maven groupId
   * @param artifactId Maven artifactId
   * @param version Maven version or null
   * @return URLClassLoader for the dependency
   * @throws IOException if JAR cannot be resolved
   */
  public URLClassLoader getLoaderForDependency(final String groupId, final String artifactId, final String version)
      throws IOException {

    final String coordinates = String.format("%s:%s:%s", groupId, artifactId, version);

    // Return cached loader if available
    if (loadersByCoordinates.containsKey(coordinates)) {
      LOGGER.debug("Returning cached loader: {}", coordinates);
      return loadersByCoordinates.get(coordinates);
    }

    // Create new loader by loading a dummy spec (this initializes the loader in DependencySpecLoader cache)
    DependencySpecLoader.loadSpec("META-INF/MANIFEST.MF", groupId, artifactId, version);

    LOGGER.debug("Created loader for dependency: {}", coordinates);
    return loadersByCoordinates.get(coordinates);
  }

  /**
   * Clears all cached specs and loaders.
   */
  public void clear() {
    LOGGER.debug("Clearing DependencyResolutionContext caches");
    specCache.clear();
    loadersByCoordinates.values().forEach(loader -> {
      try {
        loader.close();
      } catch (final IOException e) {
        LOGGER.warn("Failed to close URLClassLoader", e);
      }
    });
    loadersByCoordinates.clear();
  }

  /**
   * Gets the number of cached specs.
   *
   * @return number of cached specs
   */
  public int getCachedSpecCount() {
    return specCache.size();
  }

  /**
   * Gets the number of cached loaders.
   *
   * @return number of cached loaders
   */
  public int getCachedLoaderCount() {
    return loadersByCoordinates.size();
  }
}
