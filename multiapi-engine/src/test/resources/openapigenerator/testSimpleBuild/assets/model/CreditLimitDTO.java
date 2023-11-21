package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class CreditLimitDTO {

  @JsonProperty(value ="insuredCreditLimit")
  private InsuredCreditObjectDTO insuredCreditLimit;

  @JsonProperty(value ="isFundable")
  private Boolean isFundable;


  @Builder
  @Jacksonized
  private CreditLimitDTO(InsuredCreditObjectDTO insuredCreditLimit, Boolean isFundable) {
    this.insuredCreditLimit = insuredCreditLimit;
    this.isFundable = isFundable;

  }

}
