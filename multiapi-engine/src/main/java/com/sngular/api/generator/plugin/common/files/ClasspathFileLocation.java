/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;

@RequiredArgsConstructor
@Getter
public class ClasspathFileLocation implements FileLocation {

  private final URI path;

  @Override
  public final InputStream getFileAtLocation(final String filename) throws IOException {
    return resolveInUri(path, filename).toURL().openStream();
  }


  @Override
  public final URI getPath() {
    return URI.create(path.toString());
  }

  private static URI resolveInUri(URI parentUri, String relativePath) {
    if ("jar".equals(parentUri.getScheme())) {
      String[] parts = parentUri.getSchemeSpecificPart().split("!", 2);
      if (parts.length != 2) {
        throw new IllegalArgumentException("Invalid JAR URI: " + parentUri);
      }

      String jarPath = parts[0];
      Path parentInsideJar = Paths.get(parts[1]);
      Path resolvedPath = parentInsideJar.resolve(relativePath).normalize();

      return URI.create("jar:" + jarPath + "!" + resolvedPath.toString().replace("\\", "/"));
    } else if ("file".equals(parentUri.getScheme())) {
      Path parentPath = Paths.get(parentUri);
      Path resolved = parentPath.resolve(relativePath).normalize();
      return resolved.toUri();
    } else {
      throw new IllegalArgumentException("Unsupported URI scheme: " + parentUri.getScheme());
    }
  }
}
