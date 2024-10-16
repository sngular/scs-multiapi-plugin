package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class BooleanFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private Boolean defaultValue;


  @Builder
  @Jacksonized
  private BooleanFieldDTO(Boolean mandatory, String type, String name, Boolean defaultValue) {
    this.mandatory = mandatory;
    this.type = type;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
