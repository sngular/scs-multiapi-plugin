/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.exception;

import com.sngular.api.generator.plugin.exception.InvalidAPIException;

public class InvalidAsyncAPIException extends InvalidAPIException {

  private static final String ERROR_MESSAGE = "AsyncApi -> There is at least one operation without operationId";

  private static final String ERROR_MESSAGE_WRONG_MESSAGE_BODY = "AsyncApi -> The following operationId : %s has the wrong body";

  public InvalidAsyncAPIException() {
    super(ERROR_MESSAGE);
  }

  public InvalidAsyncAPIException(final String operationId) {
    super(String.format(ERROR_MESSAGE_WRONG_MESSAGE_BODY, operationId));
  }

}
