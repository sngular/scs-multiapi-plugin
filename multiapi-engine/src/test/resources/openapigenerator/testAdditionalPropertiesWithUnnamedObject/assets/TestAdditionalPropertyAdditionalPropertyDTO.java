package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestAdditionalPropertyAdditionalPropertyDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, Object> additionalProperties = new HashMap<String, Object>();

  @JsonProperty(value ="name")
  @NonNull
  private String name;


  @Builder
  @Jacksonized
  private TestAdditionalPropertyAdditionalPropertyDTO(Map<String, Object> additionalProperties, @NonNull String name) {
    this.additionalProperties = additionalProperties;
    this.name = name;

  }

}
