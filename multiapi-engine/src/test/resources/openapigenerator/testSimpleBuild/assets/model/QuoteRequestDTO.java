package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class QuoteRequestDTO {

  @JsonProperty(value ="debtorId")
  private Integer debtorId;

  @JsonProperty(value ="currency")
  private String currency;

  @JsonProperty(value ="merchantId")
  private Integer merchantId;

  @JsonProperty(value ="totalAmountInCents")
  private Long totalAmountInCents;

  @JsonProperty(value ="paymentDueDate")
  private String paymentDueDate;

  @JsonProperty(value ="saleDate")
  private String saleDate;


  @Builder
  @Jacksonized
  private QuoteRequestDTO(Integer debtorId, String currency, Integer merchantId, Long totalAmountInCents, String paymentDueDate, String saleDate) {
    this.debtorId = debtorId;
    this.currency = currency;
    this.merchantId = merchantId;
    this.totalAmountInCents = totalAmountInCents;
    this.paymentDueDate = paymentDueDate;
    this.saleDate = saleDate;

  }

}
