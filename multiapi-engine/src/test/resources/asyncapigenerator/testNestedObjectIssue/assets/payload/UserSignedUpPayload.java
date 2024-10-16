package com.sngular.scsplugin.nestedobject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UserSignedUpPayload {

  @JsonProperty(value ="someOtherObject")
  private SomeOtherObject someOtherObject;

  @JsonProperty(value ="email")
  private String email;

  @JsonProperty(value ="firstName")
  private String firstName;

  @JsonProperty(value ="lastName")
  private String lastName;

  @JsonProperty(value ="createdAt")
  private LocalDateTime createdAt;


  @Builder
  @Jacksonized
  private UserSignedUpPayload(SomeOtherObject someOtherObject, String email, String firstName, String lastName, LocalDateTime createdAt) {
    this.someOtherObject = someOtherObject;
    this.email = email;
    this.firstName = firstName;
    this.lastName = lastName;
    this.createdAt = createdAt;

  }

}
