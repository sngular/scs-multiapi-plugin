package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
public class UnionFieldDTO {

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

  @JsonProperty(value ="fieldDTO")
  private List<FieldDTO> fieldDTO = new ArrayList<FieldDTO>();


  @Builder
  private UnionFieldDTO(Boolean generatedFlag, String type, UnionEnum unionEnum, String name, Boolean optionalUnion, List<FieldDTO> fieldDTO) {
    this.generatedFlag = generatedFlag;
    this.type = type;
    this.unionEnum = unionEnum;
    this.name = name;
    this.optionalUnion = optionalUnion;
    this.fieldDTO = fieldDTO;

  }

}
