package com.sngular.multifileplugin.testadditionalproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class TestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, Object> additionalProperties = new HashMap<String, Object>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @JsonPOJOBuilder
  private TestDTO(Map<String, Object> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}
