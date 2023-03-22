/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Builder.Default;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import static com.sngular.api.generator.plugin.openapi.model.TypeConstants.OBJECT;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(of = "baseName")
public class SchemaFieldObject {

  private String baseName;

  @Default
  private SchemaFieldObjectType dataType = new SchemaFieldObjectType(OBJECT);

  private String importClass;

  private boolean required;

  private Map<String, String> enumValues;

}
