/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.sngular.api.generator.plugin.common.tools.PathUtil;

public record ClasspathFileLocation(URI path) implements FileLocation {

  @Override
  public InputStream getFileAtLocation(final String filename) throws IOException {
    return resolveInUri(path, filename).toURL().openStream();
  }

  private static URI resolveInUri(URI parentUri, String relativePath) {
    // If it's an absolute filesystem path: return directly
    if (PathUtil.isAbsolutePath(relativePath)) {
      return Paths.get(relativePath).toUri();
    }

    String scheme = parentUri.getScheme();

    if ("jar".equals(scheme)) {
      // Extract <jarPath> and <entryPath>
      String ssp = parentUri.getSchemeSpecificPart();
      String[] parts = ssp.split("!", 2);
      if (parts.length != 2) {
        throw new IllegalArgumentException("Invalid JAR URI: " + parentUri);
      }

      String jarPath = parts[0];  // already a valid file: URI
      String inside = parts[1].replace('\\', '/'); // force forward slashes

      // Resolve inside-JAR path using Path logic
      Path baseInsideJar = inside.isEmpty() || "/".equals(inside)
                               ? Paths.get("")             // root of jar
                               : Paths.get(inside);

      Path resolved = baseInsideJar.resolve(relativePath).normalize();

      // Build URI-safe entry path **fast**
      String entryPath = toJarEntryPath(resolved);

      return URI.create("jar:" + jarPath + "!" + entryPath);
    }

    // Normal filesystem file:
    if ("file".equals(scheme)) {
      Path parent = Paths.get(parentUri);
      Path resolved = parent.resolve(relativePath).normalize();
      return resolved.toUri();
    }

    throw new IllegalArgumentException("Unsupported URI scheme: " + scheme);
  }

  /**
   * Converts a Path to a JAR-entry-safe path string using '/' separators.
   * No streams, no iteration overhead, extremely fast.
   */
  private static String toJarEntryPath(Path path) {
    int nameCount = path.getNameCount();

    // Root of the jar → just "/"
    if (nameCount == 0) {
      return "/";
    }

    // Estimate length to reduce reallocations
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


  @Override
  public URI path() {
    return URI.create(path.toString());
  }
}
