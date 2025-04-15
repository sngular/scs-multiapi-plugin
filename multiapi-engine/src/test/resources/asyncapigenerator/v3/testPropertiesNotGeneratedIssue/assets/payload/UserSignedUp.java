package com.sngular.scsplugin.notgeneratedproperties.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UserSignedUp {

  @JsonProperty(value ="details")
  private UserDetails details;

  @JsonProperty(value ="id")
  private String id;


  @Builder
  @Jacksonized
  private UserSignedUp(UserDetails details, String id) {
    this.details = details;
    this.id = id;

  }

}
