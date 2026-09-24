package com.sngular.scsplugin.constantgeneration.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UserSignedUpPayload {

  @JsonProperty(value ="someOtherObject")
  private SomeOtherObject someOtherObject;

  @JsonProperty(value ="email")
  private String email = "je.garcia@oneemail.com";

  @JsonProperty(value ="firstName")
  private String firstName = "Jose";

  @JsonProperty(value ="lastName")
  private String lastName = "Garcia";

  @JsonProperty(value ="createdAt")
  private LocalDateTime createdAt;

  @JsonProperty(value ="numberEnum")
  private NumberEnum numberEnum;

  public enum NumberEnum {
    _1234("1234"),
    _2345("2345"),
    _3456("3456"),
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}. */
    UNKNOWN("UNKNOWN");

    private String value;

    NumberEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static NumberEnum fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (NumberEnum constant : values()) {
        if (value.equals(constant.value)) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }


  @Builder
  @Jacksonized
  private UserSignedUpPayload(SomeOtherObject someOtherObject, String email, String firstName, String lastName, LocalDateTime createdAt, NumberEnum numberEnum) {
    this.someOtherObject = someOtherObject;
    this.email = email;
    this.firstName = firstName;
    this.lastName = lastName;
    this.createdAt = createdAt;
    this.numberEnum = numberEnum;

  }

}
