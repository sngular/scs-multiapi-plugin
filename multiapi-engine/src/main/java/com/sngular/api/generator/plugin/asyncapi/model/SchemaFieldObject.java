/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.model;

import java.math.BigDecimal;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Builder.Default;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SchemaFieldObject {

  private String baseName;

  @Default
  private BigDecimal maximum = null;

  @Default
  private BigDecimal minimum = null;

  @Default
  private Boolean exclusiveMaximum = null;

  @Default
  private Boolean exclusiveMinimum = null;

  @Default
  private Boolean uniqueItems = null;

  @Default
  private Integer minItems = null;

  @Default
  private Integer maxItems = null;

  @Default
  private Integer minLength = null;

  @Default
  private Integer maxLength = null;

  @Default
  private String pattern = null;

  @Default
  private String multipleOf = null;

  private String dataTypeSimple;

  private String dataType;

  private String importClass;

  private boolean required;

  private List<String> enumValues;

}
