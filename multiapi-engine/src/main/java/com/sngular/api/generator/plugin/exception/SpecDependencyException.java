/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.exception;

/**
 * Raised when a specification declared through {@code fromGroupId}/{@code fromArtifactId} cannot be
 * resolved, unpacked, or located inside the resolved artifact.
 */
public class SpecDependencyException extends RuntimeException {

  private static final String MESSAGE = "Code generation failed.";

  public SpecDependencyException(final String message) {
    super(MESSAGE + " " + message);
  }

  public SpecDependencyException(final String message, final Throwable cause) {
    super(MESSAGE + " " + message, cause);
  }
}
