package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model.EnumSchemaDTO;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class SecondTestDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, EnumSchemaDTO> additionalProperties = new HashMap<String, EnumSchemaDTO>();

  @JsonProperty(value ="id")
  @NonNull
  private String id;


  @Builder
  @JsonPOJOBuilder
  private SecondTestDTO(Map<String, EnumSchemaDTO> additionalProperties, @NonNull String id) {
    this.additionalProperties = additionalProperties;
    this.id = id;

  }

}
