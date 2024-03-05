/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.parameter;

import java.util.List;
import java.util.Map;

import com.sngular.api.generator.plugin.common.model.IOperationObject;
import com.sngular.api.generator.plugin.common.model.TypeConstants.TimeType;
import com.sngular.api.generator.plugin.common.parameter.AbstractSpecFile;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Builder.Default;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class OpenAPIAbstractSpecFile extends AbstractSpecFile implements IOperationObject {

  private String ids;

  private String apiPackage;

  private String modelPackage;

  private String clientPackage;

  private boolean callMode;

  private boolean useTagsGroup;

  private boolean useLombokModelAnnotation;

  private boolean isReactive;

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

  @Override
  public String getModelNamePostfix() {
    return super.getModelNamePostFix();
  }

  @Override
  public String getModelNameSuffix() {
    return super.getModelNameSuffix();
  }
}


