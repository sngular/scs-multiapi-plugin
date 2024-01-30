package com.sngular.api.generator.plugin.common.model;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SchemaObject {

  protected String schemaName;

  protected String className;

  protected List<String> importList;

  private List<SchemaFieldObject> fieldObjectList;

  private String schemaCombinator;

  private String parentPackage;

  private boolean isEnum;
}
