/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.exception;

import net.coru.api.generator.plugin.exception.GeneratedSourcesFolderException;

public class AsyncApiGeneratedSourceFolderException extends GeneratedSourcesFolderException {

  public AsyncApiGeneratedSourceFolderException(final String folderName) {
    super("AsyncApi", folderName);
  }
}
