package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
public class EnumFieldDTO {

  @JsonProperty(value ="enumValues")
  private List<String> enumValues = new ArrayList<String>();

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private List<String> defaultValues = new ArrayList<String>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;


  @Builder
  private EnumFieldDTO(List<String> enumValues, String type, List<String> defaultValues, String name, String defaultValue) {
    this.enumValues = enumValues;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
