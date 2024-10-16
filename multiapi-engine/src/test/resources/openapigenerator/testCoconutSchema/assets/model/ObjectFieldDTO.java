package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ObjectFieldDTO {

  @JsonProperty(value ="requiredValues")
  @Singular("requiredValue")
  private List<String> requiredValues;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular("property")
  private List<FieldDTO> properties;

  @JsonProperty(value ="defaultValues")
  @Singular("defaultValue")
  private List<Object> defaultValues;

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
