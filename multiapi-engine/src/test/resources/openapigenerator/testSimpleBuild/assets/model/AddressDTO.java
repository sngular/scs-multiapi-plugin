package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class AddressDTO {

  @JsonProperty(value ="country")
  private String country;

  @JsonProperty(value ="city")
  private String city;

  @JsonProperty(value ="street")
  private String street;

  @JsonProperty(value ="number")
  private Integer number;

  @JsonProperty(value ="postcode")
  private Integer postcode;


  @Builder
  @Jacksonized
  private AddressDTO(String country, String city, String street, Integer number, Integer postcode) {
    this.country = country;
    this.city = city;
    this.street = street;
    this.number = number;
    this.postcode = postcode;

  }

}
