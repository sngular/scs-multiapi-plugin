package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class UnionFieldDTO {

  @JsonProperty(value ="defaultItem")
  @Singular(value = "_defaultItem", ignoreNullCollections = true)
  private List<FieldDTO> defaultItem;

  @JsonProperty(value ="generatedFlag")
  private Boolean generatedFlag;

  @JsonProperty(value ="type")
  private String type;

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
  private String name;

  @JsonProperty(value ="optionalUnion")
  private Boolean optionalUnion;

  @JsonProperty(value ="values")
  @Singular(value = "value", ignoreNullCollections = true)
  private List<FieldDTO> values;


  @Builder
  @Jacksonized
  private UnionFieldDTO(List<FieldDTO> defaultItem, Boolean generatedFlag, String type, UnionEnum unionEnum, String name, Boolean optionalUnion, List<FieldDTO> values) {
    this.defaultItem = defaultItem;
    this.generatedFlag = generatedFlag;
    this.type = type;
    this.unionEnum = unionEnum;
    this.name = name;
    this.optionalUnion = optionalUnion;
    this.values = values;

  }

}
