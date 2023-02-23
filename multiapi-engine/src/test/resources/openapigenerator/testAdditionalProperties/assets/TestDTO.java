package com.sngular.multifileplugin.testadditionalproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDTO {

  @JsonProperty(value ="additionalPropertiesMap")
  private Map<String, Object> additionalPropertiesMap = new HashMap<String, Object>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @Jacksonized
  private TestDTO(Map<String, Object> additionalPropertiesMap, @NonNull String id) {
    this.additionalPropertiesMap = additionalPropertiesMap;
    this.id = id;

  }

}
