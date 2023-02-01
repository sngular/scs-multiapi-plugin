package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
public class NumberFieldDTO {

  @JsonProperty(value ="precision")
  private Integer precision;

  @JsonProperty(value ="maximum")
  private Integer maximum;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="multipleOf")
  private Integer multipleOf;

  @JsonProperty(value ="scale")
  private Integer scale;

  @JsonProperty(value ="minimum")
  private Integer minimum;

  @JsonProperty(value ="flagExclusiveMinimum")
  private Boolean flagExclusiveMinimum;

  @JsonProperty(value ="flagExclusiveMaximum")
  private Boolean flagExclusiveMaximum;

  @JsonProperty(value ="numberEnum")
  private NumberEnum numberEnum;

  public enum NumberEnum {
    FLOAT("float"),
    INTEGER("integer");

    private String value;

    NumberEnum(String value) {
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

  @JsonProperty(value ="defaultValue")
  private Long defaultValue;


  @Builder
  @JsonPOJOBuilder
  private NumberFieldDTO(Integer precision, Integer maximum, String type, List<Object> defaultValues, String name, Integer multipleOf, Integer scale, Integer minimum, Boolean flagExclusiveMinimum, Boolean flagExclusiveMaximum, NumberEnum numberEnum, Long defaultValue) {
    this.precision = precision;
    this.maximum = maximum;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.multipleOf = multipleOf;
    this.scale = scale;
    this.minimum = minimum;
    this.flagExclusiveMinimum = flagExclusiveMinimum;
    this.flagExclusiveMaximum = flagExclusiveMaximum;
    this.numberEnum = numberEnum;
    this.defaultValue = defaultValue;

  }

}
