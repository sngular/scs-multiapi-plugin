package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class EnumFieldDTO {

  @JsonProperty(value ="enumValues")
  @Singular("enumValue")
  private List<String> enumValues;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  @Singular("defaultValue")
  private List<String> defaultValues;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;


  @Builder
  @Jacksonized
  private EnumFieldDTO(List<String> enumValues, String type, List<String> defaultValues, String name, String defaultValue) {
    this.enumValues = enumValues;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
