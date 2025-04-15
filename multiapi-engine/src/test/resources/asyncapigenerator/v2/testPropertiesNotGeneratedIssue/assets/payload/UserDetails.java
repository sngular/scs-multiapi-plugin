package com.sngular.scsplugin.notgeneratedproperties.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UserDetails {

  @JsonProperty(value ="email")
  private String email;

  @JsonProperty(value ="firstName")
  private String firstName;

  @JsonProperty(value ="lastName")
  private String lastName;


  @Builder
  @Jacksonized
  private UserDetails(String email, String firstName, String lastName) {
    this.email = email;
    this.firstName = firstName;
    this.lastName = lastName;

  }

}
