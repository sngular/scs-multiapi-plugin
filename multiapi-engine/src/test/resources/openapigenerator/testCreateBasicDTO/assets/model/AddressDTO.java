package com.sngular.multifileplugin.testCreateBasicDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class AddressDTO {

  @JsonProperty(value ="country")
  @NonNull
  private Boolean country;

  @JsonProperty(value ="city")
  @NonNull
  private String city;

  @JsonProperty(value ="street")
  private String street;


  @Builder
  @Jacksonized
  private AddressDTO(@NonNull Boolean country, @NonNull String city, String street) {
    this.country = country;
    this.city = city;
    this.street = street;

  }

}
