/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Path;

import com.sngular.api.generator.plugin.asyncapi.exception.FileSystemException;
import com.sngular.api.generator.plugin.common.tools.PathUtil;
import lombok.Getter;

@Getter
public class DirectoryFileLocation implements FileLocation {

  private final Path path;

  public DirectoryFileLocation(final Path directoryPath) {
    path = directoryPath;
  }

  @Override
  public final InputStream getFileAtLocation(final String filename) {
    try {
      // Check if filename is an absolute path
      if (PathUtil.isAbsolutePath(filename)) {
        // For absolute paths, open directly without resolving against parent
        return new FileInputStream(new File(filename));
      } else {
        // For relative paths, resolve against the parent directory
        return new FileInputStream(path.resolve(filename).toFile());
      }
    } catch (final IOException e) {
      throw new FileSystemException(e.getMessage());
    }
  }

  @Override
  public final URI path() {
    return path.toUri();
  }
}
