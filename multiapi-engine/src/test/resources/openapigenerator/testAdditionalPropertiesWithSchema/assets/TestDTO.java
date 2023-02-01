package com.sngular.multifileplugin.testadditionalpropertiesWithSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import com.sngular.multifileplugin.testadditionalpropertiesWithSchema.model.SubtestDTO;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class TestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, SubtestDTO> additionalProperties = new HashMap<String, SubtestDTO>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @JsonPOJOBuilder
  private TestDTO(Map<String, SubtestDTO> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}