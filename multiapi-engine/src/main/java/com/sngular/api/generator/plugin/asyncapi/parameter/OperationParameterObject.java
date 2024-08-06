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


import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class OperationParameterObject extends CommonSpecFile {

  private String ids;

  private String classNamePostfix;

  @SuppressWarnings("unused")
  private List<String> operationIds;

  public List<String> getOperationIds() {
    return StringUtils.isEmpty(ids) ? List.of() : List.of(ids.replace(" ", "").split(","));
  }

}
