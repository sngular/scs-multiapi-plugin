/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.exception;

public class FileParseException extends RuntimeException {

  private static final String MESSAGE = "Code generation failed";

  private static final String FILE_PARSE_EXCEPTION = "Failure trying to parse file";

  public FileParseException(final String message) {
    super(MESSAGE + " " + message);
  }

  public FileParseException(final String message, final Exception ex) {
    super(FILE_PARSE_EXCEPTION + " " + message, ex);
  }
}
