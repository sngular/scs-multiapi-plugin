package com.sngular.scsplugin.notgeneratedproperties.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserSignedUp.UserSignedUpBuilder.class)
public class UserSignedUp {

  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="details")
  private UserDetails details;

  private UserSignedUp(String id, UserDetails details) {
    this.id = id;
    this.details = details;

  }

  private UserSignedUp(UserSignedUpBuilder builder) {
    this.id = builder.id;
    this.details = builder.details;

  }

  public static UserSignedUp.UserSignedUpBuilder builder() {
    return new UserSignedUp.UserSignedUpBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpBuilder {

    private String id;
    private UserDetails details;

    public UserSignedUp.UserSignedUpBuilder id(String id) {
      this.id = id;
      return this;
    }

    public UserSignedUp.UserSignedUpBuilder details(UserDetails details) {
      this.details = details;
      return this;
    }

    public UserSignedUp build() {
      UserSignedUp userSignedUp = new UserSignedUp(this);
      return userSignedUp;
    }
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  /**
  * Get details
  * @return details
  */
  @Schema(name = "details", required = false)
  public UserDetails getDetails() {
    return details;
  }
  public void setDetails(UserDetails details) {
    this.details = details;
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
    return Objects.equals(this.id, userSignedUp.id) && Objects.equals(this.details, userSignedUp.details);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, details);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUp{");
    sb.append(" id:").append(id).append(",");
    sb.append(" details:").append(details);
    sb.append("}");
    return sb.toString();
  }




}
