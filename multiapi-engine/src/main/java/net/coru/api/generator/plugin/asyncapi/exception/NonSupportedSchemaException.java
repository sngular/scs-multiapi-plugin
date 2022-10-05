/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.exception;

public class NonSupportedSchemaException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> Schema is not supported %s.";

  public NonSupportedSchemaException(final String schema) {
    super(String.format(ERROR_MESSAGE, schema));
  }
}
