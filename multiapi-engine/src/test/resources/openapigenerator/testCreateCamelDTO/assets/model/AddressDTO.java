package com.sngular.multifileplugin.testCreateCamelDTO.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class AddressDTO {

  @JsonProperty(value ="country_name")
  @NonNull
  private String countryName;

  @JsonProperty(value ="city_name")
  @NonNull
  private String cityName;

  @JsonProperty(value ="street_name")
  private String streetName;


  @Builder
  @Jacksonized
  private AddressDTO(@NonNull String countryName, @NonNull String cityName, String streetName) {
    this.countryName = countryName;
    this.cityName = cityName;
    this.streetName = streetName;

  }

}
