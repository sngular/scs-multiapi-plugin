/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.parameter;

import com.sngular.api.generator.plugin.common.parameter.SpecFile;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor()
public class AsynAPISpecFile extends SpecFile {

  private String modelNamePrefix;

  private String modelNameSuffix;

  private OperationParameterObject supplier;

  private OperationParameterObject consumer;

  private OperationParameterObject streamBridge;
}
