package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class QuoteUpdateResponseDTO {

  @JsonProperty(value ="activateBeforeDate")
  private String activateBeforeDate;

  @JsonProperty(value ="isInsurable")
  private Boolean isInsurable;

  @JsonProperty(value ="claimBeforeDate")
  private String claimBeforeDate;

  @JsonProperty(value ="estimatedCostInCents")
  private Integer estimatedCostInCents;


  @Builder
  @Jacksonized
  private QuoteUpdateResponseDTO(String activateBeforeDate, Boolean isInsurable, String claimBeforeDate, Integer estimatedCostInCents) {
    this.activateBeforeDate = activateBeforeDate;
    this.isInsurable = isInsurable;
    this.claimBeforeDate = claimBeforeDate;
    this.estimatedCostInCents = estimatedCostInCents;

  }

}
