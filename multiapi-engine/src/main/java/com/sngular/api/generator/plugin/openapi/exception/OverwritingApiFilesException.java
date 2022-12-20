/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.exception;

public class OverwritingApiFilesException extends RuntimeException {

  private static final String ERROR_MESSAGE = "OpenApi -> At least two specFiles are configured with the same api package, and it will remain in overwrited files. Please "
                                              + "change one of them.";

  public OverwritingApiFilesException() {
    super(ERROR_MESSAGE);
  }
}
