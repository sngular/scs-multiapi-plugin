package com.corunet.api.generator.plugin.openapi.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ParameterObject {

  String name;
  String in;
  Boolean required;
  String description;
  Boolean isCollection;
  String className;
}
