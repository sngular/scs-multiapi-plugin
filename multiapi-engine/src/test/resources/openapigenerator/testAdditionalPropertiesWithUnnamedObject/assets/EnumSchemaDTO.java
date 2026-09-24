package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum EnumSchemaDTO {
  ASC("asc"),
  DESC("desc"),
  /** A value the contract does not declare, written back as {@code "UNKNOWN"}. */
  UNKNOWN("UNKNOWN");

  private String value;

  EnumSchemaDTO(String value) {
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
  public static EnumSchemaDTO fromValue(String value) {
    if (value == null) {
      return null;
    }
    for (EnumSchemaDTO constant : values()) {
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