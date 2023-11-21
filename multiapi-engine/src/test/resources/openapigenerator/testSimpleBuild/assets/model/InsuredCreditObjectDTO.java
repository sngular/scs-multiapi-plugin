package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class InsuredCreditObjectDTO {

  @JsonProperty(value ="creditLimitInCents")
  private Integer creditLimitInCents;

  @JsonProperty(value ="providerName")
  private String providerName;

  @JsonProperty(value ="creditAvailableInCents")
  private Integer creditAvailableInCents;


  @Builder
  @Jacksonized
  private InsuredCreditObjectDTO(Integer creditLimitInCents, String providerName, Integer creditAvailableInCents) {
    this.creditLimitInCents = creditLimitInCents;
    this.providerName = providerName;
    this.creditAvailableInCents = creditAvailableInCents;

  }

}
