/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.exception;

public class OperationException extends RuntimeException {

  private static final String ERROR_MESSAGE = "%s -> There are at least two operations that share OperationID: %s";

  public OperationException(final String sourceModule, final String message) {
    super(String.format(ERROR_MESSAGE, sourceModule, message));
  }
}
