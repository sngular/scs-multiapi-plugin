/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ParameterObject {

  private String name;

  private String in;

  private Boolean required;

  private String description;

  private Boolean isCollection;

  private SchemaFieldObjectType dataType;

  private String importName;

  /**
   * The name under which this parameter is declared in the generated Java code. It is the contract name whenever that name is a legal Java identifier, and a
   * sanitized version of it otherwise - a header named {@code Idempotency-Key} is declared as {@code idempotencyKey}. The contract name stays in {@link #name}
   * and is what the generated code sends and binds against.
   */
  public String getVariableName() {
    return StringCaseUtils.toJavaVariableName(name);
  }
}
