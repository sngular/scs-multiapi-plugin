package com.sngular.scsplugin.notgeneratedproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserDetails.UserDetailsBuilder.class)
public class UserDetails {

  @JsonProperty(value ="firstName")
  private String firstName;
  @JsonProperty(value ="lastName")
  private String lastName;
  @JsonProperty(value ="email")
  private String email;

  private UserDetails(String firstName, String lastName, String email) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.email = email;

  }

  private UserDetails(UserDetailsBuilder builder) {
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.email = builder.email;

  }

  public static UserDetails.UserDetailsBuilder builder() {
    return new UserDetails.UserDetailsBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserDetailsBuilder {

    private String firstName;
    private String lastName;
    private String email;

    public UserDetails.UserDetailsBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public UserDetails.UserDetailsBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public UserDetails.UserDetailsBuilder email(String email) {
      this.email = email;
      return this;
    }

    public UserDetails build() {
      UserDetails details = new UserDetails(this);
      return details;
    }
  }

  /**
  * Get firstName
  * @return firstName
  */
  @Schema(name = "firstName", required = false)
  public String getFirstName() {
    return firstName;
  }
  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  /**
  * Get lastName
  * @return lastName
  */
  @Schema(name = "lastName", required = false)
  public String getLastName() {
    return lastName;
  }
  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  /**
  * Get email
  * @return email
  */
  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UserDetails details = (UserDetails) o;
    return Objects.equals(this.firstName, details.firstName) && Objects.equals(this.lastName, details.lastName) && Objects.equals(this.email, details.email);
  }

  @Override
  public int hashCode() {
    return Objects.hash(firstName, lastName, email);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserDetails{");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" email:").append(email);
    sb.append("}");
    return sb.toString();
  }


}
