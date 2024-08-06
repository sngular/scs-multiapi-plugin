package com.sngular.scsplugin.notgeneratedproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserDetails.UserDetailsBuilder.class)
public class UserDetails {

  @JsonProperty(value ="email")
  private String email;
  @JsonProperty(value ="firstName")
  private String firstName;
  @JsonProperty(value ="lastName")
  private String lastName;

  private UserDetails(UserDetailsBuilder builder) {
    this.email = builder.email;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;

  }

  public static UserDetails.UserDetailsBuilder builder() {
    return new UserDetails.UserDetailsBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserDetailsBuilder {

    private String email;
    private String firstName;
    private String lastName;

    public UserDetails.UserDetailsBuilder email(String email) {
      this.email = email;
      return this;
    }

    public UserDetails.UserDetailsBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public UserDetails.UserDetailsBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public UserDetails build() {
      UserDetails userDetails = new UserDetails(this);
      return userDetails;
    }
  }

  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  @Schema(name = "firstName", required = false)
  public String getFirstName() {
    return firstName;
  }
  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  @Schema(name = "lastName", required = false)
  public String getLastName() {
    return lastName;
  }
  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UserDetails userDetails = (UserDetails) o;
    return Objects.equals(this.email, userDetails.email) && Objects.equals(this.firstName, userDetails.firstName) && Objects.equals(this.lastName, userDetails.lastName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(email, firstName, lastName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserDetails{");
    sb.append(" email:").append(email).append(",");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName);
    sb.append("}");
    return sb.toString();
  }


}
