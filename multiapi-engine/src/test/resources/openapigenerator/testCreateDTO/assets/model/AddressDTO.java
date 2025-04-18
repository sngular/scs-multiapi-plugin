package com.sngular.multifileplugin.testCreateDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class AddressDTO {

  @JsonProperty(value ="country")
  @NonNull
  private String country;

  @JsonProperty(value ="city")
  @NonNull
  private String city;

  @JsonProperty(value ="street")
  private String street;


  @Builder
  @Jacksonized
  private AddressDTO(@NonNull String country, @NonNull String city, String street) {
    this.country = country;
    this.city = city;
    this.street = street;

  }

}
