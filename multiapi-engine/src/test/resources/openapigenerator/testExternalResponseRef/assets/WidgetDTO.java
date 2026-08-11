package com.sngular.multifileplugin.testexternalresponseref.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class WidgetDTO {

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="id")
  private String id;


  @Builder
  @Jacksonized
  private WidgetDTO(String name, String id) {
    this.name = name;
    this.id = id;

  }

}