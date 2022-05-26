package com.corunet.api.generator.plugin.openapi.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ContentObject {

  private String name;
  private String description;
  private String refName;
  private String typeData;
  private String importName;
  private SchemaObject schemaObject;

}
