/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.exception;

import net.coru.api.generator.plugin.exception.OperationException;

public class DuplicatedOperationException extends OperationException {

  public DuplicatedOperationException(final String message) {
    super("OpenApi", message);
  }
}
