package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ObjectFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="requiredValues")
  @Singular(value = "requiredValue", ignoreNullCollections = true)
  private List<String> requiredValues;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="properties")
  @Singular(value = "property", ignoreNullCollections = true)
  private List<FieldDTO> properties;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="defaultValue")
  @Singular(value = "_defaultValue", ignoreNullCollections = true)
  private List<Object> defaultValue;


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
