/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.parameter;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public final class OperationParameterObject {

  private String ids;

  private String apiPackage;

  private String modelPackage;

  private String modelNameSuffix;

  private String classNamePostfix;

  private boolean useLombokModelAnnotation;

  private List<String> operationIds;

  public List<String> getOperationIds() {
    return StringUtils.isEmpty(ids) ? List.of() : List.of(ids.replace(" ", "").split(","));
  }

}
