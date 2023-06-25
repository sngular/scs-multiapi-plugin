package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class UnionFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="defaultItem")
  private FieldDTO defaultItem;

  @JsonProperty(value ="generatedFlag")
  private Boolean generatedFlag;

  @JsonProperty(value ="type")
  private String type;

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
  private String name;

  @JsonProperty(value ="optionalUnion")
  private Boolean optionalUnion;

  @JsonProperty(value ="values")
  private List<FieldDTO> values = new ArrayList<FieldDTO>();


  @Builder
  @Jacksonized
  private UnionFieldDTO(Boolean mandatory, FieldDTO defaultItem, Boolean generatedFlag, String type, UnionEnum unionEnum, String name, Boolean optionalUnion, List<FieldDTO> values) {
    this.mandatory = mandatory;
    this.defaultItem = defaultItem;
    this.generatedFlag = generatedFlag;
    this.type = type;
    this.unionEnum = unionEnum;
    this.name = name;
    this.optionalUnion = optionalUnion;
    this.values = values;

  }

}
