package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class EnumFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="enumValues")
  private List<String> enumValues = new ArrayList<String>();

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;


  @Builder
  @Jacksonized
  private EnumFieldDTO(Boolean mandatory, List<String> enumValues, String type, String name, String defaultValue) {
    this.mandatory = mandatory;
    this.enumValues = enumValues;
    this.type = type;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
