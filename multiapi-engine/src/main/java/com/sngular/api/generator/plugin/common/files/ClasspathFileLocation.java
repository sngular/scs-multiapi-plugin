/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.files;

import java.io.InputStream;

import com.sngular.api.generator.plugin.asyncapi.AsyncApiGenerator;

public class ClasspathFileLocation implements FileLocation {

  private final String path;

  public ClasspathFileLocation(final String child) {
    path = child.substring(0, child.lastIndexOf('/') + 1);
  }

  @Override
  public InputStream getFileAtLocation(final String filename) {
    return AsyncApiGenerator.class.getClassLoader().getResourceAsStream(path + filename.replace("./", ""));
  }
}
