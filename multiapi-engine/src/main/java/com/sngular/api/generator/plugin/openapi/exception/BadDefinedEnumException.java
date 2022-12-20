/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.exception;

public class BadDefinedEnumException extends RuntimeException {

  private static final String ERROR_MESSAGE = "OpenApi -> Enum called %s is incorrectly defined.";

  public BadDefinedEnumException(final String enumName) {
    super(String.format(ERROR_MESSAGE, enumName));
  }
}
