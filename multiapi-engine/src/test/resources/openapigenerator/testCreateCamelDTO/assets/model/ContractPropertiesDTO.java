package com.sngular.multifileplugin.testCreateCamelDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ContractPropertiesDTO {

  @JsonProperty(value ="may_value")
  private String mayValue;

  @JsonProperty(value ="may_key")
  private String mayKey;


  @Builder
  @Jacksonized
  private ContractPropertiesDTO(String mayValue, String mayKey) {
    this.mayValue = mayValue;
    this.mayKey = mayKey;

  }

}
