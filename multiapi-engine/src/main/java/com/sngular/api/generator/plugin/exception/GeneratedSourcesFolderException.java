/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.exception;

public class GeneratedSourcesFolderException extends RuntimeException {

  private static final String ERROR_MESSAGE = "%s -> The selected name for the generated sources folder includes prohibited characters: %s";

  public GeneratedSourcesFolderException(final String sourceModule, final String folderName) {
    super(String.format(ERROR_MESSAGE, sourceModule, folderName));
  }
}
