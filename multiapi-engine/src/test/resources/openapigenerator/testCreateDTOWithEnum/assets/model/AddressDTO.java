package com.sngular.multifileplugin.testCreateDTOWithEnum.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class AddressDTO {

  @JsonProperty(value ="country")
  @NonNull
  private Country country;

  public enum Country {
    COUNTRY_ES("Country ES"),
    COUNTRY_TR("Country TR"),
    COUNTRY_EN("Country EN"),
    COUNTRY_PT("Country PT"),
    UNKNOWN("UNKNOWN");

    private String value;

    Country(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static Country fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (Country constant : values()) {
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

  @JsonProperty(value ="city")
  @NonNull
  private String city;

  @JsonProperty(value ="street")
  private String street;


  @Builder
  @Jacksonized
  private AddressDTO(@NonNull Country country, @NonNull String city, String street) {
    this.country = country;
    this.city = city;
    this.street = street;

  }

}
