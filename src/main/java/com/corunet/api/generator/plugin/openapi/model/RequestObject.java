package com.corunet.api.generator.plugin.openapi.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RequestObject {

  private String description;
  private Boolean required;
  private List<ContentObject> contentObject;

}
