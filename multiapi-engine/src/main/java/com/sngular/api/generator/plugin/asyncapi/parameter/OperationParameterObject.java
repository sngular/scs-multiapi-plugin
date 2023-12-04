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
import java.util.Map;

import com.sngular.api.generator.plugin.common.model.TimeType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Builder.Default;
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

  @Builder.Default
  private String dateTimeFormat = "yyyy-MM-dd'T'HH:mm:ss";

  @Builder.Default
  private String dateFormat = "yyyy-MM-dd";
  
  @Default
  private TimeType useTimeType = TimeType.LOCAL;

  @SuppressWarnings("unused")
  private List<String> operationIds;

  public List<String> getOperationIds() {
    return StringUtils.isEmpty(ids) ? List.of() : List.of(ids.replace(" ", "").split(","));
  }
  
  public Map<String, String> getFormats() {
    return Map.of("DATE_TIME", dateTimeFormat, "DATE", dateFormat);
  }

}
