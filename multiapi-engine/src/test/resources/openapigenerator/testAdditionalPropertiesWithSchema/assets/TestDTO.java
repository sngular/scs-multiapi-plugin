package com.sngular.multifileplugin.testadditionalpropertiesWithSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, SubtestDTO> additionalProperties = new HashMap<String, SubtestDTO>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @Jacksonized
  private TestDTO(Map<String, SubtestDTO> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}