package com.sngular.scsplugin.constantgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserSignedUpPayload.UserSignedUpPayloadBuilder.class)
public class UserSignedUpPayload {

  @JsonProperty(value ="firstName")
  private final String firstName;
  @JsonProperty(value ="lastName")
  private final String lastName;
  @JsonProperty(value ="email")
  private final String email;
  @JsonProperty(value ="createdAt")
  private String createdAt;
  @JsonProperty(value ="someOtherObject")
  private SomeOtherObject someOtherObject;

  private UserSignedUpPayload(UserSignedUpPayloadBuilder builder) {
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;
    this.email = builder.email;
    this.createdAt = builder.createdAt;
    this.someOtherObject = builder.someOtherObject;

  }

  public static UserSignedUpPayload.UserSignedUpPayloadBuilder builder() {
    return new UserSignedUpPayload.UserSignedUpPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpPayloadBuilder {

    private String firstName = "Jose";

    private String lastName = "Garcia";

    private String email = "je.garcia@oneemail.com";

    private String createdAt;

    private SomeOtherObject someOtherObject;

    public UserSignedUpPayload.UserSignedUpPayloadBuilder createdAt(String createdAt) {
      this.createdAt = createdAt;
      return this;
    }

    public UserSignedUpPayload.UserSignedUpPayloadBuilder someOtherObject(SomeOtherObject someOtherObject) {
      this.someOtherObject = someOtherObject;
      return this;
    }

    public UserSignedUpPayload build() {
      UserSignedUpPayload userSignedUpPayload = new UserSignedUpPayload(this);
      return userSignedUpPayload;
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

  /**
  * Get lastName
  * @return lastName
  */
  @Schema(name = "lastName", required = false)
  public String getLastName() {
    return lastName;
  }

  /**
  * Get email
  * @return email
  */
  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }

  /**
  * Get createdAt
  * @return createdAt
  */
  @Schema(name = "createdAt", required = false)
  public String getCreatedAt() {
    return createdAt;
  }
  public void setCreatedAt(String createdAt) {
    this.createdAt = createdAt;
  }

  /**
  * Get someOtherObject
  * @return someOtherObject
  */
  @Schema(name = "someOtherObject", required = false)
  public SomeOtherObject getSomeOtherObject() {
    return someOtherObject;
  }
  public void setSomeOtherObject(SomeOtherObject someOtherObject) {
    this.someOtherObject = someOtherObject;
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
    return Objects.equals(this.firstName, userSignedUpPayload.firstName) && Objects.equals(this.lastName, userSignedUpPayload.lastName) && Objects.equals(this.email, userSignedUpPayload.email) && Objects.equals(this.createdAt, userSignedUpPayload.createdAt) && Objects.equals(this.someOtherObject, userSignedUpPayload.someOtherObject);
  }

  @Override
  public int hashCode() {
    return Objects.hash(firstName, lastName, email, createdAt, someOtherObject);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUpPayload{");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName).append(",");
    sb.append(" email:").append(email).append(",");
    sb.append(" createdAt:").append(createdAt).append(",");
    sb.append(" someOtherObject:").append(someOtherObject);
    sb.append("}");
    return sb.toString();
  }


}
