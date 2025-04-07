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
import java.nio.file.Path;

@RequiredArgsConstructor
@Getter
public class ClasspathFileLocation implements FileLocation {

  private final Path path;

  @Override
  public final InputStream getFileAtLocation(final String filename) throws IOException {
    return path.resolve(filename).toUri().toURL().openStream();
  }
}
