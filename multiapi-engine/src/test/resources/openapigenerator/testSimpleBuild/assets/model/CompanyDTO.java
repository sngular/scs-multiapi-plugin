package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class CompanyDTO {

  @JsonProperty(value ="fiscalAddress")
  private AddressDTO fiscalAddress;

  @JsonProperty(value ="isInsurable")
  private Boolean isInsurable;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="isActive")
  private Boolean isActive;


  @Builder
  @Jacksonized
  private CompanyDTO(AddressDTO fiscalAddress, Boolean isInsurable, String name, Boolean isActive) {
    this.fiscalAddress = fiscalAddress;
    this.isInsurable = isInsurable;
    this.name = name;
    this.isActive = isActive;

  }

}
