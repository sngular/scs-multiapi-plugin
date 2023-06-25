package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ArrayFieldDTO {

  @JsonProperty(value ="uniqueItems")
  private Boolean uniqueItems;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private List<String> defaultValues = new ArrayList<String>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="arraySize")
  private Integer arraySize;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="minItems")
  private Integer minItems;

  @JsonProperty(value ="values")
  private List<FieldDTO> values = new ArrayList<FieldDTO>();


  @Builder
  @Jacksonized
  private ArrayFieldDTO(Boolean uniqueItems, String type, List<String> defaultValues, String name, Integer arraySize, String regex, Integer minItems, List<FieldDTO> values) {
    this.uniqueItems = uniqueItems;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.arraySize = arraySize;
    this.regex = regex;
    this.minItems = minItems;
    this.values = values;

  }

}
