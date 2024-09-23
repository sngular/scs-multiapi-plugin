package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class StringFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="maxLength")
  private Integer maxLength;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<String> properties;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="minLength")
  private Integer minLength;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  @JsonProperty(value ="format")
  private String format;

  @JsonProperty(value ="valueLength")
  private Integer valueLength;


  @Builder
  @Jacksonized
  private StringFieldDTO(Boolean mandatory, Integer maxLength, String type, List<String> properties, String name, String regex, Integer minLength, String defaultValue, String format, Integer valueLength) {
    this.mandatory = mandatory;
    this.maxLength = maxLength;
    this.type = type;
    this.properties = properties;
    this.name = name;
    this.regex = regex;
    this.minLength = minLength;
    this.defaultValue = defaultValue;
    this.format = format;
    this.valueLength = valueLength;

  }

}
