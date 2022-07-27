/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.exception;

public class ExternalRefComponentNotFoundException extends RuntimeException {

  private static final String ERROR_MESSAGE = "AsyncApi -> External referenced component %s not found in selected file %s";

  public ExternalRefComponentNotFoundException(final String component, final String file) {
    super(String.format(ERROR_MESSAGE, component, file));
  }
}
