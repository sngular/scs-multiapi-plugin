package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ObjectFieldDTO {

  @JsonProperty(value ="requiredValues")
  private List<String> requiredValues = new ArrayList<String>();

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  private List<FieldDTO> properties = new ArrayList<FieldDTO>();

  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();

  @JsonProperty(value ="name")
  private String name;


  @Builder
  @Jacksonized
  private ObjectFieldDTO(List<String> requiredValues, String type, List<FieldDTO> properties, List<Object> defaultValues, String name) {
    this.requiredValues = requiredValues;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.name = name;

  }

}
