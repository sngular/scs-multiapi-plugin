/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SchemaObject {

  private boolean isEnum;

  private String schemaName;

  private String className;

  private List<String> importList;

  private Set<SchemaFieldObject> fieldObjectList;

  private String schemaCombinator;

  public static final class SchemaObjectBuilder {

    private boolean isEnum = false;

    private final List<String> importList = new ArrayList<>();

    private final Set<SchemaFieldObject> fieldObjectList = new HashSet<>();

    public SchemaObjectBuilder importList(final List<String> importList) {
      this.importList.addAll(importList);
      return this;
    }

    public SchemaObjectBuilder importItem(final String importItem) {
      this.importList.add(importItem);
      return this;
    }

    public SchemaObjectBuilder fieldObjectList(final List<SchemaFieldObject> fieldObjectList) {
      this.fieldObjectList.addAll(fieldObjectList);
      return this;
    }

    public SchemaObjectBuilder fieldObject(final SchemaFieldObject fieldObject) {
      this.fieldObjectList.add(fieldObject);
      return this;
    }

    public SchemaObjectBuilder isEnum(final boolean isEnum) {
      this.isEnum = isEnum;
      return this;
    }
  }
}
