package com.sngular.multifileplugin.enumlombokgeneration.model;

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
    ALLOF("allof");

    private String value;

    UnionEnum(String value) {
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
    LONG_3(3l);

    private Long value;

    UnionIntegerEnum(Long value) {
      this.value = value;
    }

    @JsonValue
    public Long getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  @JsonProperty(value ="unionNumberEnum")
  private UnionNumberEnum unionNumberEnum;

  public enum UnionNumberEnum {
    BIG_DECIMAL_1_DOT_1(new BigDecimal("1.1")),
    BIG_DECIMAL_2_DOT_2(new BigDecimal("2.2")),
    BIG_DECIMAL_4_DOT_4(new BigDecimal("4.4"));

    private BigDecimal value;

    UnionNumberEnum(BigDecimal value) {
      this.value = value;
    }

    @JsonValue
    public BigDecimal getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
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
