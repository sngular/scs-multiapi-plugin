package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model.TestAdditionalPropertyDTO;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class TestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, TestAdditionalPropertyDTO> additionalProperties = new HashMap<String, TestAdditionalPropertyDTO>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  private TestDTO(Map<String, TestAdditionalPropertyDTO> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}
