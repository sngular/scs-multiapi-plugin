package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ResponseObject {

  private String responseName;
  private String description;
  private List<ContentObject> contentObject;
}
