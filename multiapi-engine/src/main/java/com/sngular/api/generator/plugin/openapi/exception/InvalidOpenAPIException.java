/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.exception;

import com.sngular.api.generator.plugin.exception.InvalidAPIException;

public class InvalidOpenAPIException extends InvalidAPIException {

  private static final String ERROR_MESSAGE = "OpenApi -> There is at least one operation without operationId";

  public InvalidOpenAPIException() {
    super(ERROR_MESSAGE);
  }

}
