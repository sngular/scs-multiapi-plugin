package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ActivatePolicyDTO {

  @JsonProperty(value ="provider")
  private String provider;


  @Builder
  @Jacksonized
  private ActivatePolicyDTO(String provider) {
    this.provider = provider;

  }

}
