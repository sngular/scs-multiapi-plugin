/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Builder.Default;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SchemaFieldObject {

  private String baseName;

  private String dataTypeSimple;

  @Default
  private SchemaFieldObjectProperties restrictions = new SchemaFieldObjectProperties();

  private String dataType;

  private String importClass;

  private boolean required;

  private List<String> enumValues;

  private String parentPackage;

}
