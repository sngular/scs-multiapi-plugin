package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class SchemaObject {

  private String description;

  private String schemaName;

  private String className;

  private List<String> importList;

  private List<SchemaFieldObject> fieldObjectList;



}
