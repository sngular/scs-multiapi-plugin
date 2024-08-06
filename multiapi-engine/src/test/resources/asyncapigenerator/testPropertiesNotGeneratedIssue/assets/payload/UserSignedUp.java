package com.sngular.scsplugin.notgeneratedproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserSignedUp.UserSignedUpBuilder.class)
public class UserSignedUp {

  @JsonProperty(value ="details")
  private UserDetails details;
  @JsonProperty(value ="id")
  private String id;

  private UserSignedUp(UserSignedUpBuilder builder) {
    this.details = builder.details;
    this.id = builder.id;

  }

  public static UserSignedUp.UserSignedUpBuilder builder() {
    return new UserSignedUp.UserSignedUpBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpBuilder {

    private UserDetails details;
    private String id;
    public UserSignedUp.UserSignedUpBuilder details(UserDetails details) {
      this.details = details;
      return this;
    }

    public UserSignedUp.UserSignedUpBuilder id(String id) {
      this.id = id;
      return this;
    }

    public UserSignedUp build() {
      UserSignedUp userSignedUp = new UserSignedUp(this);
      return userSignedUp;
    }
  }

  @Schema(name = "details", required = false)
  public UserDetails getDetails() {
    return details;
  }
  public void setDetails(UserDetails details) {
    this.details = details;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UserSignedUp userSignedUp = (UserSignedUp) o;
    return Objects.equals(this.details, userSignedUp.details) && Objects.equals(this.id, userSignedUp.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(details, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUp{");
    sb.append(" details:").append(details).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
