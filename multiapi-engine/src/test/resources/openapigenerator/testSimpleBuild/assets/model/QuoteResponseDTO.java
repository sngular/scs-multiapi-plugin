package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class QuoteResponseDTO {

  @JsonProperty(value ="activateBeforeDate")
  private String activateBeforeDate;

  @JsonProperty(value ="isInsurable")
  private Boolean isInsurable;

  @JsonProperty(value ="claimBeforeDate")
  private String claimBeforeDate;

  @JsonProperty(value ="currency")
  private String currency;

  @JsonProperty(value ="estimatedCostInCents")
  private Integer estimatedCostInCents;

  @JsonProperty(value ="quoteId")
  private String quoteId;


  @Builder
  @Jacksonized
  private QuoteResponseDTO(String activateBeforeDate, Boolean isInsurable, String claimBeforeDate, String currency, Integer estimatedCostInCents, String quoteId) {
    this.activateBeforeDate = activateBeforeDate;
    this.isInsurable = isInsurable;
    this.claimBeforeDate = claimBeforeDate;
    this.currency = currency;
    this.estimatedCostInCents = estimatedCostInCents;
    this.quoteId = quoteId;

  }

}
