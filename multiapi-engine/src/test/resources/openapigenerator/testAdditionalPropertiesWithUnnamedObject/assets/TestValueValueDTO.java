package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestValueValueDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, List<ArraySchemaDTO>> additionalProperties = new HashMap<String, List<ArraySchemaDTO>>();

  @JsonProperty(value ="name")
  @NonNull
  private String name;


  @Builder
  @Jacksonized
  private TestValueValueDTO(Map<String, List<ArraySchemaDTO>> additionalProperties, @NonNull String name) {
    this.additionalProperties = additionalProperties;
    this.name = name;

  }

}
