package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PathObject {

  private String pathName;
  private GlobalObject globalObjects;
  private List<OperationObject> operationObject;
}
