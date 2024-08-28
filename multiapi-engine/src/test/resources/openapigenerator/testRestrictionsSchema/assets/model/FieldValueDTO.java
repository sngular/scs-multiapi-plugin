package com.sngular.multifileplugin.testRestrictionsSchema.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;
import com.sngular.multifileplugin.testRestrictionsSchema.model.exception.ModelClassException;

@Value
public class FieldValueDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="precision")
  private Integer precision;

  @JsonProperty(value ="defaultItem")
  private FieldDTO defaultItem;

  @JsonProperty(value ="maximum")
  private Integer maximum;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="scale")
  private Integer scale;

  @JsonProperty(value ="minLength")
  private Integer minLength;

  @JsonProperty(value ="increment")
  private Integer increment;

  @JsonProperty(value ="keyType")
  private String keyType;

  @JsonProperty(value ="requiredValues")
  private List<String> requiredValues = new ArrayList<String>();

  @JsonProperty(value ="optionalUnion")
  private Boolean optionalUnion;

  @JsonProperty(value ="initialValue")
  private FieldDTO initialValue;

  @JsonProperty(value ="flagExclusiveMinimum")
  private Boolean flagExclusiveMinimum;

  @JsonProperty(value ="flagExclusiveMaximum")
  private Boolean flagExclusiveMaximum;

  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  @JsonProperty(value ="property")
  private FieldDTO property;

  @JsonProperty(value ="enumValues")
  private List<String> enumValues = new ArrayList<String>();

  @JsonProperty(value ="properties")
  private List<String> properties = new ArrayList<String>();

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

  @JsonProperty(value ="seqEnum")
  private SeqEnum seqEnum;

  public enum SeqEnum {
    MONTH("MONTH"),
    YEAR("YEAR"),
    HOUR("HOUR"),
    MINUTE("MINUTE"),
    SECOND("SECOND"),
    DAY("DAY");

    private String value;

    SeqEnum(String value) {
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

  @JsonProperty(value ="arraySize")
  private Integer arraySize;

  @JsonProperty(value ="multipleOf")
  private Integer multipleOf;

  @JsonProperty(value ="mapSize")
  private Integer mapSize;

  @JsonProperty(value ="mapTypes")
  private List<FieldDTO> mapTypes = new ArrayList<FieldDTO>();

  @JsonProperty(value ="format")
  private String format;

  @JsonProperty(value ="generatedFlag")
  private Boolean generatedFlag;

  @JsonProperty(value ="maxLength")
  private Integer maxLength;

  @JsonProperty(value ="uniqueItems")
  private Boolean uniqueItems;

  @JsonProperty(value ="elements")
  private Integer elements;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="regex")
  private String regex;

  @JsonProperty(value ="minItems")
  private Integer minItems;

  @JsonProperty(value ="values")
  private List<FieldDTO> values = new ArrayList<FieldDTO>();

  @JsonProperty(value ="minimum")
  private Integer minimum;

  @JsonProperty(value ="numberEnum")
  private NumberEnum numberEnum;

  public enum NumberEnum {
    BYTES("bytes"),
    FLOAT("float"),
    FIXED("fixed"),
    DECIMAL("decimal"),
    INTEGER("integer"),
    LONG("long");

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

  @JsonProperty(value ="valueLength")
  private Integer valueLength;


  @Builder
  @Jacksonized
  private FieldValueDTO(Boolean mandatory, Integer precision, FieldDTO defaultItem, Integer maximum, String type, Integer scale, Integer minLength, Integer increment, String keyType, List<String> requiredValues, Boolean optionalUnion, FieldDTO initialValue, Boolean flagExclusiveMinimum, Boolean flagExclusiveMaximum, String defaultValue, FieldDTO property, List<String> enumValues, List<String> properties, UnionEnum unionEnum, SeqEnum seqEnum, Integer arraySize, Integer multipleOf, Integer mapSize, List<FieldDTO> mapTypes, String format, Boolean generatedFlag, Integer maxLength, Boolean uniqueItems, Integer elements, String name, String regex, Integer minItems, List<FieldDTO> values, Integer minimum, NumberEnum numberEnum, Integer valueLength) {
    this.mandatory = mandatory;
    this.precision = precision;
    this.defaultItem = defaultItem;
    this.maximum = maximum;
    this.type = type;
    this.scale = scale;
    this.minLength = minLength;
    this.increment = increment;
    this.keyType = keyType;
    this.requiredValues = requiredValues;
    this.optionalUnion = optionalUnion;
    this.initialValue = initialValue;
    this.flagExclusiveMinimum = flagExclusiveMinimum;
    this.flagExclusiveMaximum = flagExclusiveMaximum;
    this.defaultValue = defaultValue;
    this.property = property;
    this.enumValues = enumValues;
    this.properties = properties;
    this.unionEnum = unionEnum;
    this.seqEnum = seqEnum;
    this.arraySize = arraySize;
    this.multipleOf = multipleOf;
    this.mapSize = mapSize;
    this.mapTypes = mapTypes;
    this.format = format;
    this.generatedFlag = generatedFlag;
    this.maxLength = maxLength;
    this.uniqueItems = uniqueItems;
    this.elements = elements;
    this.name = name;
    this.regex = regex;
    this.minItems = minItems;
    this.values = values;
    this.minimum = minimum;
    this.numberEnum = numberEnum;
    this.valueLength = valueLength;

    validatePartialCombinations();
  }

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.mandatory)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.precision)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.defaultItem)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.maximum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.type)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.scale)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.minLength)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.increment)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.keyType)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.requiredValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.optionalUnion)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.initialValue)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.flagExclusiveMinimum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.flagExclusiveMaximum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.defaultValue)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.property)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.enumValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.properties)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.unionEnum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.seqEnum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.arraySize)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.multipleOf)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.mapSize)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.mapTypes)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.format)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.generatedFlag)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.maxLength)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.uniqueItems)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.elements)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.regex)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.minItems)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.values)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.minimum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.numberEnum)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.valueLength)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("FieldValueDTO");
    }
  }
}
