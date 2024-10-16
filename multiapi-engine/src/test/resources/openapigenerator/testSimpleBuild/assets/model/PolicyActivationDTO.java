package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class PolicyActivationDTO {

  @JsonProperty(value ="policyId")
  private String policyId;

  @JsonProperty(value ="claimBeforeDate")
  private String claimBeforeDate;

  @JsonProperty(value ="netCostInCents")
  private Integer netCostInCents;

  @JsonProperty(value ="totalCostInCents")
  private Integer totalCostInCents;

  @JsonProperty(value ="isActive")
  private Boolean isActive;

  @JsonProperty(value ="currency")
  private String currency;

  @JsonProperty(value ="coveredAmountInCents")
  private Integer coveredAmountInCents;

  @JsonProperty(value ="insuranceProvider")
  private String insuranceProvider;


  @Builder
  @Jacksonized
  private PolicyActivationDTO(String policyId, String claimBeforeDate, Integer netCostInCents, Integer totalCostInCents, Boolean isActive, String currency, Integer coveredAmountInCents, String insuranceProvider) {
    this.policyId = policyId;
    this.claimBeforeDate = claimBeforeDate;
    this.netCostInCents = netCostInCents;
    this.totalCostInCents = totalCostInCents;
    this.isActive = isActive;
    this.currency = currency;
    this.coveredAmountInCents = coveredAmountInCents;
    this.insuranceProvider = insuranceProvider;

  }

}
