package com.sngular.scsplugin.nestedobject.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;

@JsonDeserialize(builder = UserSignedUpPayload.UserSignedUpPayloadBuilder.class)
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

  private UserSignedUpPayload(UserSignedUpPayloadBuilder builder) {
    this.someOtherObject = builder.someOtherObject;
    this.email = builder.email;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.createdAt = builder.createdAt;

  }

  public static UserSignedUpPayload.UserSignedUpPayloadBuilder builder() {
    return new UserSignedUpPayload.UserSignedUpPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpPayloadBuilder {

    private SomeOtherObject someOtherObject;
    private String email;
    private String firstName;
    private String lastName;
    private LocalDateTime createdAt;

    public UserSignedUpPayload.UserSignedUpPayloadBuilder someOtherObject(SomeOtherObject someOtherObject) {
      this.someOtherObject = someOtherObject;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder email(String email) {
      this.email = email;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder createdAt(LocalDateTime createdAt) {
      this.createdAt = createdAt;
      return this;
    }

    public UserSignedUpPayload build() {
      UserSignedUpPayload userSignedUpPayload = new UserSignedUpPayload(this);
      return userSignedUpPayload;
    }
  }

  @Schema(name = "someOtherObject", required = false)
  public SomeOtherObject getSomeOtherObject() {
    return someOtherObject;
  }
  public void setSomeOtherObject(SomeOtherObject someOtherObject) {
    this.someOtherObject = someOtherObject;
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

  @Schema(name = "createdAt", required = false)
  public LocalDateTime getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(LocalDateTime createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UserSignedUpPayload userSignedUpPayload = (UserSignedUpPayload) o;
    return Objects.equals(this.someOtherObject, userSignedUpPayload.someOtherObject) && Objects.equals(this.email, userSignedUpPayload.email) && Objects.equals(this.firstName, userSignedUpPayload.firstName) && Objects.equals(this.lastName, userSignedUpPayload.lastName) && Objects.equals(this.createdAt, userSignedUpPayload.createdAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someOtherObject, email, firstName, lastName, createdAt);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUpPayload{");
    sb.append(" someOtherObject:").append(someOtherObject).append(",");
    sb.append(" email:").append(email).append(",");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" createdAt:").append(createdAt);
    sb.append("}");
    return sb.toString();
  }


}
