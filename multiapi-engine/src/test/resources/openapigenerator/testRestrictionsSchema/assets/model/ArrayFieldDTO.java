package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ArrayFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="uniqueItems")
  private Boolean uniqueItems;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="arraySize")
  private Integer arraySize;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="minItems")
  private Integer minItems;

  @JsonProperty(value ="values")
  @Singular("value")
  private List<FieldDTO> values;

  @JsonProperty(value ="defaultValue")
  @Singular("_defaultValue")
  private List<FieldDTO> defaultValue;


  @Builder
  @Jacksonized
  private ArrayFieldDTO(Boolean mandatory, Boolean uniqueItems, String type, String name, Integer arraySize, String regex, Integer minItems, List<FieldDTO> values, List<FieldDTO> defaultValue) {
    this.mandatory = mandatory;
    this.uniqueItems = uniqueItems;
    this.type = type;
    this.name = name;
    this.arraySize = arraySize;
    this.regex = regex;
    this.minItems = minItems;
    this.values = values;
    this.defaultValue = defaultValue;

  }

}
