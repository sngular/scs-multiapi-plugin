package com.sngular.multifileplugin.testadditionalproperties.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.NonNull;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="additionalProperties")
  @Singular("additionalProperty")
  private Map<String, Object> additionalProperties;

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @Jacksonized
  private TestDTO(Map<String, Object> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}
