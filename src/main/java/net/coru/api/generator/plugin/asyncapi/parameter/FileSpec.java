/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.parameter;

public class FileSpec {

  private String filePath;

  private OperationParameterObject supplier;

  private OperationParameterObject consumer;

  private OperationParameterObject streamBridge;

  public final String getFilePath() {
    return filePath;
  }

  public final OperationParameterObject getSupplier() {
    return supplier;
  }

  public final OperationParameterObject getConsumer() {
    return consumer;
  }

  public final OperationParameterObject getStreamBridge() {
    return streamBridge;
  }
}
