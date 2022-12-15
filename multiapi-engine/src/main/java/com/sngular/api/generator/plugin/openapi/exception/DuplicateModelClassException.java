/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.exception;

public class DuplicateModelClassException extends RuntimeException {

  private static final String ERROR_MESSAGE = "OpenApi -> There are at least two model classes with the same name %s and package %s. Please modify one of them or turn on "
                                              + "OverWriteModel parameter";

  public DuplicateModelClassException(final String className, final String packageName) {
    super(String.format(ERROR_MESSAGE, className, packageName));
  }
}
