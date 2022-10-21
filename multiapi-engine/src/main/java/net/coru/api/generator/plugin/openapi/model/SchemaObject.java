/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SchemaObject {

  private String schemaName;

  private String className;

  private List<String> importList = new ArrayList<>();

  private Set<SchemaFieldObject> fieldObjectList = new HashSet<>();

  private String schemaCombinator;

  public static final class SchemaObjectBuilder {

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
  }
}
