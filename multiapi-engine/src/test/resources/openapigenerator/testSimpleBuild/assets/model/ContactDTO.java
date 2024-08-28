package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class ContactDTO {

  @JsonProperty(value ="email")
  @NonNull
  private String email;

  @JsonProperty(value ="phoneNumber")
  @NonNull
  private Integer phoneNumber;

  @JsonProperty(value ="firstName")
  private String firstName;

  @JsonProperty(value ="lastName")
  @NonNull
  private Integer lastName;

  @JsonProperty(value ="officePhoneNumber")
  @NonNull
  private Integer officePhoneNumber;


  @Builder
  @Jacksonized
  private ContactDTO(@NonNull String email, @NonNull Integer phoneNumber, String firstName, @NonNull Integer lastName, @NonNull Integer officePhoneNumber) {
    this.email = email;
    this.phoneNumber = phoneNumber;
    this.firstName = firstName;
    this.lastName = lastName;
    this.officePhoneNumber = officePhoneNumber;

  }

}
