/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.asyncapi.parameter;

import lombok.Builder;
import lombok.Data;
import org.apache.maven.plugins.annotations.Parameter;

@Data
public class FileSpec {

  @Parameter(name = "filePath", property = "filePath", required = true)
  String filePath;

  @Parameter(name = "supplier", property = "supplier")
  OperationParameterObject supplier;

  @Parameter(name = "consumer", property = "consumer")
  OperationParameterObject consumer;

  @Parameter(name = "streamBridge", property = "streamBridge")
  OperationParameterObject streamBridge;

}
