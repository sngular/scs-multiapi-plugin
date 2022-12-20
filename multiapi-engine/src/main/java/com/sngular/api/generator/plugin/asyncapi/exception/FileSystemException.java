/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.exception;

import java.io.IOException;

public class FileSystemException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> File System error trying to create necessary folder %s";

  private static final String IO_ERROR_MESSAGE = "AsyncApi -> File System error trying to manipulate files: %s";

  public FileSystemException(final String folderName) {
    super(String.format(ERROR_MESSAGE, folderName));
  }

  public FileSystemException(final IOException ioException) {
    super(String.format(IO_ERROR_MESSAGE, ioException.getMessage()), ioException);
  }

  public FileSystemException(final Exception exception) {
    super(String.format(IO_ERROR_MESSAGE, exception.getMessage()), exception);

  }
}
