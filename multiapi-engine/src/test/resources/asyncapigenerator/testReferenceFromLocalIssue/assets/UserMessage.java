package com.github.issue.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserMessage.UserMessageBuilder.class)
public class UserMessage {

  @JsonProperty(value ="firstName")
  private String firstName;
  @JsonProperty(value ="lastName")
  private String lastName;

  private UserMessage(UserMessageBuilder builder) {
    this.firstName = builder.firstName;
    this.lastName = builder.lastName;

  }

  public static UserMessage.UserMessageBuilder builder() {
    return new UserMessage.UserMessageBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserMessageBuilder {

    private String firstName;
    private String lastName;

    public UserMessage.UserMessageBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public UserMessage.UserMessageBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public UserMessage build() {
      UserMessage userMessage = new UserMessage(this);
      return userMessage;
    }
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
    UserMessage userMessage = (UserMessage) o;
    return Objects.equals(this.firstName, userMessage.firstName) && Objects.equals(this.lastName, userMessage.lastName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(firstName, lastName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserMessage{");
    sb.append(" firstName:").append(firstName).append(",");
    sb.append(" lastName:").append(lastName);
    sb.append("}");
    return sb.toString();
  }


}
