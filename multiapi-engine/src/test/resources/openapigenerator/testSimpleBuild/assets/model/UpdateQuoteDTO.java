package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UpdateQuoteDTO {

  @JsonProperty(value ="invoiceDate")
  @NonNull
  private String invoiceDate;

  @JsonProperty(value ="provider")
  private String provider;

  @JsonProperty(value ="invoiceNumber")
  @NonNull
  private String invoiceNumber;


  @Builder
  @Jacksonized
  private UpdateQuoteDTO(@NonNull String invoiceDate, String provider, @NonNull String invoiceNumber) {
    this.invoiceDate = invoiceDate;
    this.provider = provider;
    this.invoiceNumber = invoiceNumber;

  }

}
