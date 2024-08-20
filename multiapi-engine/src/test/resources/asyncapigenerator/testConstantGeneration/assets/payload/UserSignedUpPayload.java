package com.sngular.scsplugin.constantgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;

@JsonDeserialize(builder = UserSignedUpPayload.UserSignedUpPayloadBuilder.class)
public class UserSignedUpPayload {

  @JsonProperty(value ="someOtherObject")
  private SomeOtherObject someOtherObject;
  @JsonProperty(value ="email")
  private final String email;
  @JsonProperty(value ="firstName")
  private final String firstName;
  @JsonProperty(value ="lastName")
  private final String lastName;
  @JsonProperty(value ="createdAt")
  private LocalDateTime createdAt;
  @JsonProperty(value ="numberEnum")
  private NumberEnum numberEnum;
  public enum NumberEnum {
    _1234("1234"),
    _2345("2345"),
    _3456("3456");

    private String value;

    NumberEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  private UserSignedUpPayload(UserSignedUpPayloadBuilder builder) {
    this.someOtherObject = builder.someOtherObject;
    this.email = builder.email;
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.createdAt = builder.createdAt;
    this.numberEnum = builder.numberEnum;

  }

  public static UserSignedUpPayload.UserSignedUpPayloadBuilder builder() {
    return new UserSignedUpPayload.UserSignedUpPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpPayloadBuilder {

    private SomeOtherObject someOtherObject;
    private String email = "je.garcia@oneemail.com";
    private String firstName = "Jose";
    private String lastName = "Garcia";
    private LocalDateTime createdAt;
    private NumberEnum numberEnum;

    public UserSignedUpPayload.UserSignedUpPayloadBuilder someOtherObject(SomeOtherObject someOtherObject) {
      this.someOtherObject = someOtherObject;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder createdAt(LocalDateTime createdAt) {
      this.createdAt = createdAt;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder numberEnum(NumberEnum numberEnum) {
      this.numberEnum = numberEnum;
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

  @Schema(name = "firstName", required = false)
  public String getFirstName() {
    return firstName;
  }

  @Schema(name = "lastName", required = false)
  public String getLastName() {
    return lastName;
  }

  @Schema(name = "createdAt", required = false)
  public LocalDateTime getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(LocalDateTime createdAt) {
    this.createdAt = createdAt;
  }

  @Schema(name = "numberEnum", required = false)
  public NumberEnum getNumberEnum() {
    return numberEnum;
  }
  public void setNumberEnum(NumberEnum numberEnum) {
    this.numberEnum = numberEnum;
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
    return Objects.equals(this.someOtherObject, userSignedUpPayload.someOtherObject) && Objects.equals(this.email, userSignedUpPayload.email) && Objects.equals(this.firstName, userSignedUpPayload.firstName) && Objects.equals(this.lastName, userSignedUpPayload.lastName) && Objects.equals(this.createdAt, userSignedUpPayload.createdAt) && Objects.equals(this.numberEnum, userSignedUpPayload.numberEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someOtherObject, email, firstName, lastName, createdAt, numberEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUpPayload{");
    sb.append(" someOtherObject:").append(someOtherObject).append(",");
    sb.append(" email:").append(email).append(",");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" createdAt:").append(createdAt).append(",");
    sb.append(" numberEnum:").append(numberEnum);
    sb.append("}");
    return sb.toString();
  }


}
