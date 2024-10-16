package com.sngular.scsplugin.infiniteLoop.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ConfigDTO {

  @JsonProperty(value ="name")
  private String name;


  @Builder
  @Jacksonized
  private ConfigDTO(String name) {
    this.name = name;

  }

}
