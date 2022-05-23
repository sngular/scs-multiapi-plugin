package com.corunet.api.generator.plugin.openapi.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class SchemaFieldObject {

  private String baseName;

  private String dataTypeSimple;

  private String dataType;

  private String importClass;

  private String required;


}
