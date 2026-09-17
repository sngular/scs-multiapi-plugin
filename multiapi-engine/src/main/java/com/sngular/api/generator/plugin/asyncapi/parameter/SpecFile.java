/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.parameter;

import com.sngular.api.generator.plugin.common.model.ExternalSpecSource;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class SpecFile implements ExternalSpecSource {

  private String filePath;

  /**
   * Coordinates of the artifact that publishes the contract. When set, {@link #filePath} is read
   * from inside that artifact instead of from the module's filesystem. See {@link ExternalSpecSource}.
   */
  private String fromGroupId;

  private String fromArtifactId;

  private String fromVersion;

  private OperationParameterObject supplier;

  private OperationParameterObject consumer;

  private OperationParameterObject streamBridge;

  @Builder.Default
  private boolean generateModelOnly = false;

}
