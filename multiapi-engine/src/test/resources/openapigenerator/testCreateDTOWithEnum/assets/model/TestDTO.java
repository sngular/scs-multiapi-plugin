package com.sngular.multifileplugin.testCreateDTOWithEnum.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="properties")
  @NonNull
  private Properties properties;

  public enum Properties {
    ENUM_VALUE_1("Enum Value 1"),
    ENUM_VALUE_3("Enum Value 3"),
    ENUM_VALUE_2("Enum Value 2");

    private String value;

    Properties(String value) {
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

  @JsonProperty(value ="id")
  @NonNull
  private String id;

  @JsonProperty(value ="address")
  private AddressDTO address;

  @JsonProperty(value ="age")
  @NonNull
  private BigDecimal age;


  @Builder
  @Jacksonized
  private TestDTO(@NonNull Properties properties, @NonNull String id, AddressDTO address, @NonNull BigDecimal age) {
    this.properties = properties;
    this.id = id;
    this.address = address;
    this.age = age;

  }

}
