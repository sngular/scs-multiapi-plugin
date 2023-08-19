package com.sngular.scsplugin.nestedobject.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = UserSignedUp.UserSignedUpBuilder.class)
public class UserSignedUp {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload payload;

  private UserSignedUp(com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload payload) {
    this.payload = payload;

  }

  private UserSignedUp(UserSignedUpBuilder builder) {
    this.payload = builder.payload;

  }

  public static UserSignedUp.UserSignedUpBuilder builder() {
    return new UserSignedUp.UserSignedUpBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class UserSignedUpBuilder {

    private com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload payload;

    public UserSignedUp.UserSignedUpBuilder payload(com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload payload) {
      this.payload = payload;
      return this;
    }

    public UserSignedUp build() {
      UserSignedUp userSignedUp = new UserSignedUp(this);
      return userSignedUp;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.nestedobject.model.schemas.UserSignedUpPayload payload) {
    this.payload = payload;
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
    return Objects.equals(this.payload, userSignedUp.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("UserSignedUp{");
    sb.append(" payload:").append(payload);
    sb.append("}");
    return sb.toString();
  }




}
