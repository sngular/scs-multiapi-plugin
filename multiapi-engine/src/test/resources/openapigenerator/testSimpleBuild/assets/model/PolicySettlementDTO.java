package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class PolicySettlementDTO {

  @JsonProperty(value ="provider")
  private String provider;

  @JsonProperty(value ="settlementDate")
  private String settlementDate;


  @Builder
  @Jacksonized
  private PolicySettlementDTO(String provider, String settlementDate) {
    this.provider = provider;
    this.settlementDate = settlementDate;

  }

}
