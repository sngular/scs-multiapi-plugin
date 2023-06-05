package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class DateFieldDTO {

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private Object defaultValues;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="format")
  private String format;


  @Builder
  @Jacksonized
  private DateFieldDTO(String type, Object defaultValues, String name, String format) {
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.format = format;

  }

}
