package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
public class BooleanFieldDTO {

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private Boolean defaultValue;


  @Builder
  @JsonPOJOBuilder
  private BooleanFieldDTO(String type, String name, Boolean defaultValue) {
    this.type = type;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
