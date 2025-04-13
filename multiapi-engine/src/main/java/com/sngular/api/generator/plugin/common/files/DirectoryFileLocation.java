/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import lombok.Getter;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Path;

@Getter
public class DirectoryFileLocation implements FileLocation {

  private final Path path;

  public DirectoryFileLocation(final Path directoryPath) {
    path = directoryPath;
  }

  @Override
  public final InputStream getFileAtLocation(final String filename) {
    try {
      return new FileInputStream(path.resolve(filename).toFile());
    } catch (final IOException e) {
      throw new FileSystemException(e.getMessage());
    }
  }

  @Override
  public final URI getPath() {
    return path.toUri();
  }
}
