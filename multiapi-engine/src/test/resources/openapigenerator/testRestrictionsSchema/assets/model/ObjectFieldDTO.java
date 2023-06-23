package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ObjectFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="requiredValues")
  private List<String> requiredValues = new ArrayList<String>();

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  private List<FieldDTO> properties = new ArrayList<FieldDTO>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  private List<Object> defaultValue = new ArrayList<Object>();


  @Builder
  @Jacksonized
  private ObjectFieldDTO(Boolean mandatory, List<String> requiredValues, String type, List<FieldDTO> properties, String name, List<Object> defaultValue) {
    this.mandatory = mandatory;
    this.requiredValues = requiredValues;
    this.type = type;
    this.properties = properties;
    this.name = name;
    this.defaultValue = defaultValue;

  }

}
