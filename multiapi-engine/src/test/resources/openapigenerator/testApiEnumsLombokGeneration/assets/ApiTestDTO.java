package com.sngular.multifileplugin.enumlombokgeneration.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.math.BigDecimal;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ApiTestDTO {

  @JsonProperty(value ="unionEnum")
  private UnionEnum unionEnum;

  public enum UnionEnum {
    ONEOF("oneof"),
    ANYOF("anyof"),
    ALLOF("allof"),
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}. */
    UNKNOWN("UNKNOWN");

    private String value;

    UnionEnum(String value) {
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
    public static UnionEnum fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (UnionEnum constant : values()) {
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

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;

  @JsonProperty(value ="unionIntegerEnum")
  private UnionIntegerEnum unionIntegerEnum;

  public enum UnionIntegerEnum {
    LONG_1(1l),
    LONG_2(2l),
    LONG_3(3l),
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}; {@link #getValue()} returns {@code null}. */
    UNKNOWN(null);

    private Long value;

    UnionIntegerEnum(Long value) {
      this.value = value;
    }

    /** The contract value; {@code null} for {@link #UNKNOWN}. */
    public Long getValue() {
      return value;
    }

    @JsonValue
    public Object toJsonValue() {
      return this == UNKNOWN ? "UNKNOWN" : value;
    }

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static UnionIntegerEnum fromValue(Object value) {
      if (value == null) {
        return null;
      }
      final java.math.BigDecimal number;
      try {
        number = new java.math.BigDecimal(value.toString());
      } catch (NumberFormatException e) {
        return UNKNOWN;
      }
      for (UnionIntegerEnum constant : values()) {
        if (constant.value != null && number.compareTo(new java.math.BigDecimal(constant.value.toString())) == 0) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return this == UNKNOWN ? "UNKNOWN" : String.valueOf(value);
    }
  }

  @JsonProperty(value ="unionNumberEnum")
  private UnionNumberEnum unionNumberEnum;

  public enum UnionNumberEnum {
    BIG_DECIMAL_1_DOT_1(new BigDecimal("1.1")),
    BIG_DECIMAL_2_DOT_2(new BigDecimal("2.2")),
    BIG_DECIMAL_4_DOT_4(new BigDecimal("4.4")),
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}; {@link #getValue()} returns {@code null}. */
    UNKNOWN(null);

    private BigDecimal value;

    UnionNumberEnum(BigDecimal value) {
      this.value = value;
    }

    /** The contract value; {@code null} for {@link #UNKNOWN}. */
    public BigDecimal getValue() {
      return value;
    }

    @JsonValue
    public Object toJsonValue() {
      return this == UNKNOWN ? "UNKNOWN" : value;
    }

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static UnionNumberEnum fromValue(Object value) {
      if (value == null) {
        return null;
      }
      final java.math.BigDecimal number;
      try {
        number = new java.math.BigDecimal(value.toString());
      } catch (NumberFormatException e) {
        return UNKNOWN;
      }
      for (UnionNumberEnum constant : values()) {
        if (constant.value != null && number.compareTo(new java.math.BigDecimal(constant.value.toString())) == 0) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return this == UNKNOWN ? "UNKNOWN" : String.valueOf(value);
    }
  }


  @Builder
  @Jacksonized
  private ApiTestDTO(UnionEnum unionEnum, @NonNull String name, @NonNull Integer id, UnionIntegerEnum unionIntegerEnum, UnionNumberEnum unionNumberEnum) {
    this.unionEnum = unionEnum;
    this.name = name;
    this.id = id;
    this.unionIntegerEnum = unionIntegerEnum;
    this.unionNumberEnum = unionNumberEnum;

  }

}
