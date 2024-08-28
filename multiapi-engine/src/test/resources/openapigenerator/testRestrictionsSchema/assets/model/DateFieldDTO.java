package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class DateFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  @JsonProperty(value ="format")
  private String format;


  @Builder
  @Jacksonized
  private DateFieldDTO(Boolean mandatory, String type, String name, String defaultValue, String format) {
    this.mandatory = mandatory;
    this.type = type;
    this.name = name;
    this.defaultValue = defaultValue;
    this.format = format;

  }

}
