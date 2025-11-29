/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.exception;

public class InvalidAvroException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> Avro schema at path %s lacks a namespace.";

  public InvalidAvroException(final String enumName) {
    super(String.format(ERROR_MESSAGE, enumName));
  }
}