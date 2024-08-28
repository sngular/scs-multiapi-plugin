package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class SecondTestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, EnumSchemaDTO> additionalProperties = new HashMap<String, EnumSchemaDTO>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @Jacksonized
  private SecondTestDTO(Map<String, EnumSchemaDTO> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}
