package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class StringFieldDTO {

  @JsonProperty(value ="maxLength")
  private Integer maxLength;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<String> properties;

  @JsonProperty(value ="defaultValues")
  @Singular("defaultValue")
  private List<String> defaultValues;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="minLength")
  private Integer minLength;

  @JsonProperty(value ="format")
  private String format;

  @JsonProperty(value ="valueLength")
  private Integer valueLength;


  @Builder
  @Jacksonized
  private StringFieldDTO(Integer maxLength, String type, List<String> properties, List<String> defaultValues, String name, String regex, Integer minLength, String format, Integer valueLength) {
    this.maxLength = maxLength;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.name = name;
    this.regex = regex;
    this.minLength = minLength;
    this.format = format;
    this.valueLength = valueLength;

  }

}
