package com.sngular.scsplugin.notgeneratedproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Details.DetailsBuilder.class)
public class Details {

  @JsonProperty(value ="firstName")
  private String firstName;
  @JsonProperty(value ="lastName")
  private String lastName;
  @JsonProperty(value ="email")
  private String email;

  private Details(String firstName, String lastName, String email) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.email = email;

  }

  private Details(DetailsBuilder builder) {
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.email = builder.email;

  }

  public static Details.DetailsBuilder builder() {
    return new Details.DetailsBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DetailsBuilder {

    private String firstName;
    private String lastName;
    private String email;

    public Details.DetailsBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public Details.DetailsBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public Details.DetailsBuilder email(String email) {
      this.email = email;
      return this;
    }

    public Details build() {
      Details details = new Details(this);
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
    Details details = (Details) o;
    return Objects.equals(this.firstName, details.firstName) && Objects.equals(this.lastName, details.lastName) && Objects.equals(this.email, details.email);
  }

  @Override
  public int hashCode() {
    return Objects.hash(firstName, lastName, email);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Details{");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" email:").append(email);
    sb.append("}");
    return sb.toString();
  }


}
