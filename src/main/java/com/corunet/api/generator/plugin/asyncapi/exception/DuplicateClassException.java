/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.corunet.api.generator.plugin.asyncapi.exception;

public class DuplicateClassException extends RuntimeException {

  private static final String ERROR_MESSAGE = "There are at least two classes with the same name %s and package %s";

  public DuplicateClassException(final String className, final String packageName) {
    super(String.format(ERROR_MESSAGE, className, packageName));
  }
}
