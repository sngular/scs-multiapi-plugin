/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.exception;

public class FileSystemException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> File System error trying to create necessary folder %s";

  public FileSystemException(final String folderName) {
    super(String.format(ERROR_MESSAGE, folderName));
  }
}
