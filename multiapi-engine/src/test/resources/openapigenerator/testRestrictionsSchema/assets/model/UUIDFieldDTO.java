package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UUIDFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  @JsonProperty(value ="format")
  private String format;


  @Builder
  @Jacksonized
  private UUIDFieldDTO(Boolean mandatory, String type, String name, String regex, String defaultValue, String format) {
    this.mandatory = mandatory;
    this.type = type;
    this.name = name;
    this.regex = regex;
    this.defaultValue = defaultValue;
    this.format = format;

  }

}
