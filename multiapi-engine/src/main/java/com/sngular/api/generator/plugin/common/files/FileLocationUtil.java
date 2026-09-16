/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Utility class for resolving parent URIs across different file location types.
 * Handles filesystem paths, JAR entries, and remote URLs uniformly.
 *
 * This is essential for correctly resolving external references ($ref) in API specs
 * when they are loaded from classpath resources (e.g., from dependency JARs).
 */
public final class FileLocationUtil {

  private FileLocationUtil() {
  }

  /**
   * Extracts the parent directory URI from any type of file URI.
   * Properly handles JAR URIs so that relative references within JARs can be resolved.
   *
   * @param uri The file URI (e.g., jar:file:/path/to/app.jar!/com/example/openapi.yml or file:///path/to/openapi.yml)
   * @return The parent directory URI in the same format as the input
   * @throws IllegalArgumentException if the URI format is invalid or has no parent
   */
  public static URI getParentUri(final URI uri) throws IllegalArgumentException {
    if ("jar".equals(uri.getScheme())) {
      return getParentUriFromJar(uri);
    } else if ("file".equals(uri.getScheme())) {
      return getParentUriFromFile(uri);
    }
    throw new IllegalArgumentException("Unsupported URI scheme: " + uri.getScheme());
  }

  /**
   * Extracts parent from a JAR URI.
   * Example: jar:file:/app/lib.jar!/com/example/openapi.yml → jar:file:/app/lib.jar!/com/example
   *
   * @param uri The JAR URI
   * @return The parent directory URI within the JAR
   * @throws IllegalArgumentException if the URI format is invalid or has no parent
   */
  private static URI getParentUriFromJar(final URI uri) throws IllegalArgumentException {
    String ssp = uri.getSchemeSpecificPart();
    String[] parts = ssp.split("!", 2);

    if (parts.length != 2) {
      throw new IllegalArgumentException("Invalid JAR URI: " + uri);
    }

    String jarPath = parts[0];  // e.g., "file:/path/to/app.jar"
    Path innerPath = Paths.get(parts[1]);  // e.g., "/com/example/openapi.yml"
    Path parentPath = innerPath.getParent();

    if (parentPath == null) {
      throw new IllegalArgumentException("No parent path inside JAR for: " + uri);
    }

    String entryPath = toJarEntryPath(parentPath);
    return URI.create("jar:" + jarPath + "!" + entryPath);
  }

  /**
   * Extracts parent from a filesystem file URI.
   * Example: file:///path/to/openapi.yml → file:///path/to
   *
   * @param uri The file URI
   * @return The parent directory URI
   * @throws IllegalArgumentException if the URI has no parent
   */
  private static URI getParentUriFromFile(final URI uri) throws IllegalArgumentException {
    Path path = Paths.get(uri);
    Path parent = path.getParent();

    if (parent == null) {
      throw new IllegalArgumentException("No parent for file URI: " + uri);
    }

    return parent.toUri();
  }

  /**
   * Converts a Path to a JAR-entry-safe path string using '/' separators.
   * No streams, no iteration overhead, extremely fast.
   *
   * Example: Paths.get("com/example") → "/com/example"
   *
   * @param path The path to convert
   * @return The JAR-entry path string with leading "/" and "/" separators
   */
  private static String toJarEntryPath(final Path path) {
    int nameCount = path.getNameCount();

    if (nameCount == 0) {
      return "/";
    }

    StringBuilder sb = new StringBuilder(path.toString().length() + nameCount + 2);
    sb.append('/');

    for (int i = 0; i < nameCount; i++) {
      sb.append(path.getName(i));
      if (i < nameCount - 1) {
        sb.append('/');
      }
    }

    return sb.toString();
  }
}
