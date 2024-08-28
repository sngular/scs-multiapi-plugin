package com.sngular.multifileplugin.testCreateDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class PropertiesDTO {

  @JsonProperty(value ="value")
  private String value;

  @JsonProperty(value ="key")
  private String key;


  @Builder
  @Jacksonized
  private PropertiesDTO(String value, String key) {
    this.value = value;
    this.key = key;

  }

}
