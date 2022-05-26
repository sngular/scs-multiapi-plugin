/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.asyncapi.exception;

public class DuplicatedOperationException extends RuntimeException {

  public DuplicatedOperationException(final String message) {
    super("There are at least two operations that share OperationID: " + message);
  }
}
