package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

@JsonDeserialize(builder = ApiTypeArrayDTO.ApiTypeArrayDTOBuilder.class)
public class ApiTypeArrayDTO {

  @JsonProperty(value ="precision")
  private Integer precision;
  @JsonProperty(value ="defaultItem")
  private ApiDefaultItemDTO defaultItem;
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
  private String initialValue;
  @JsonProperty(value ="flagExclusiveMinimum")
  private Boolean flagExclusiveMinimum;
  @JsonProperty(value ="flagExclusiveMaximum")
  private Boolean flagExclusiveMaximum;
  @JsonProperty(value ="defaultValue")
  private Boolean defaultValue;
  @JsonProperty(value ="enumValues")
  private List<String> enumValues = new ArrayList<String>();
  @JsonProperty(value ="properties")
  private List<String> properties = new ArrayList<String>();
  @JsonProperty(value ="defaultValues")
  private List<String> defaultValues = new ArrayList<String>();
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
  private List<ApiTypeArrayDTO> mapTypes = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="format")
  private Integer format;
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
  private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="minimum")
  private Integer minimum;
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
  @JsonProperty(value ="valueLength")
  private Integer valueLength;

  private ApiTypeArrayDTO(Integer precision, ApiDefaultItemDTO defaultItem, Integer maximum, String type, Integer scale, Integer minLength, Integer increment, String keyType, List<String> requiredValues, Boolean optionalUnion, String initialValue, Boolean flagExclusiveMinimum, Boolean flagExclusiveMaximum, Boolean defaultValue, List<String> enumValues, List<String> properties, List<String> defaultValues, UnionEnum unionEnum, SeqEnum seqEnum, Integer arraySize, Integer multipleOf, Integer mapSize, List<ApiTypeArrayDTO> mapTypes, Integer format, Boolean generatedFlag, Integer maxLength, Boolean uniqueItems, Integer elements, String name, String regex, Integer minItems, List<ApiTypeArrayDTO> values, Integer minimum, NumberEnum numberEnum, Integer valueLength) {
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
    this.enumValues = enumValues;
    this.properties = properties;
    this.defaultValues = defaultValues;
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

  private ApiTypeArrayDTO(ApiTypeArrayDTOBuilder builder) {
    this.precision = builder.precision;
    this.defaultItem = builder.defaultItem;
    this.maximum = builder.maximum;
    this.type = builder.type;
    this.scale = builder.scale;
    this.minLength = builder.minLength;
    this.increment = builder.increment;
    this.keyType = builder.keyType;
    this.requiredValues = builder.requiredValues;
    this.optionalUnion = builder.optionalUnion;
    this.initialValue = builder.initialValue;
    this.flagExclusiveMinimum = builder.flagExclusiveMinimum;
    this.flagExclusiveMaximum = builder.flagExclusiveMaximum;
    this.defaultValue = builder.defaultValue;
    this.enumValues = builder.enumValues;
    this.properties = builder.properties;
    this.defaultValues = builder.defaultValues;
    this.unionEnum = builder.unionEnum;
    this.seqEnum = builder.seqEnum;
    this.arraySize = builder.arraySize;
    this.multipleOf = builder.multipleOf;
    this.mapSize = builder.mapSize;
    this.mapTypes = builder.mapTypes;
    this.format = builder.format;
    this.generatedFlag = builder.generatedFlag;
    this.maxLength = builder.maxLength;
    this.uniqueItems = builder.uniqueItems;
    this.elements = builder.elements;
    this.name = builder.name;
    this.regex = builder.regex;
    this.minItems = builder.minItems;
    this.values = builder.values;
    this.minimum = builder.minimum;
    this.numberEnum = builder.numberEnum;
    this.valueLength = builder.valueLength;

    validatePartialCombinations();
  }

  public static ApiTypeArrayDTO.ApiTypeArrayDTOBuilder builder() {
    return new ApiTypeArrayDTO.ApiTypeArrayDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTypeArrayDTOBuilder {

    private Integer precision;
    private ApiDefaultItemDTO defaultItem;
    private Integer maximum;
    private String type;
    private Integer scale;
    private Integer minLength;
    private Integer increment;
    private String keyType;
    private List<String> requiredValues = new ArrayList<String>();
    private Boolean optionalUnion;
    private String initialValue;
    private Boolean flagExclusiveMinimum;
    private Boolean flagExclusiveMaximum;
    private Boolean defaultValue;
    private List<String> enumValues = new ArrayList<String>();
    private List<String> properties = new ArrayList<String>();
    private List<String> defaultValues = new ArrayList<String>();
    private UnionEnum unionEnum;
    private SeqEnum seqEnum;
    private Integer arraySize;
    private Integer multipleOf;
    private Integer mapSize;
    private List<ApiTypeArrayDTO> mapTypes = new ArrayList<ApiTypeArrayDTO>();
    private Integer format;
    private Boolean generatedFlag;
    private Integer maxLength;
    private Boolean uniqueItems;
    private Integer elements;
    private String name;
    private String regex;
    private Integer minItems;
    private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();
    private Integer minimum;
    private NumberEnum numberEnum;
    private Integer valueLength;

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder precision(Integer precision) {
      this.precision = precision;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultItem(ApiDefaultItemDTO defaultItem) {
      this.defaultItem = defaultItem;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder maximum(Integer maximum) {
      this.maximum = maximum;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder scale(Integer scale) {
      this.scale = scale;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder minLength(Integer minLength) {
      this.minLength = minLength;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder increment(Integer increment) {
      this.increment = increment;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder keyType(String keyType) {
      this.keyType = keyType;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder requiredValues(List<String> requiredValues) {
      if (!requiredValues.isEmpty()) {
        this.requiredValues.addAll(requiredValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder requiredValue(String requiredValue) {
      if (requiredValue != null) {
        this.requiredValues.add(requiredValue);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder optionalUnion(Boolean optionalUnion) {
      this.optionalUnion = optionalUnion;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder initialValue(String initialValue) {
      this.initialValue = initialValue;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder flagExclusiveMinimum(Boolean flagExclusiveMinimum) {
      this.flagExclusiveMinimum = flagExclusiveMinimum;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder flagExclusiveMaximum(Boolean flagExclusiveMaximum) {
      this.flagExclusiveMaximum = flagExclusiveMaximum;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultValue(Boolean defaultValue) {
      this.defaultValue = defaultValue;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder enumValues(List<String> enumValues) {
      if (!enumValues.isEmpty()) {
        this.enumValues.addAll(enumValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder enumValue(String enumValue) {
      if (enumValue != null) {
        this.enumValues.add(enumValue);
      }
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder properties(List<String> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder propertie(String propertie) {
      if (propertie != null) {
        this.properties.add(propertie);
      }
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultValues(List<String> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultValue(String defaultValue) {
      if (defaultValue != null) {
        this.defaultValues.add(defaultValue);
      }
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder unionEnum(UnionEnum unionEnum) {
      this.unionEnum = unionEnum;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder seqEnum(SeqEnum seqEnum) {
      this.seqEnum = seqEnum;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder arraySize(Integer arraySize) {
      this.arraySize = arraySize;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder multipleOf(Integer multipleOf) {
      this.multipleOf = multipleOf;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder mapSize(Integer mapSize) {
      this.mapSize = mapSize;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder mapTypes(List<ApiTypeArrayDTO> mapTypes) {
      if (!mapTypes.isEmpty()) {
        this.mapTypes.addAll(mapTypes);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder mapType(ApiTypeArrayDTO mapType) {
      if (mapType != null) {
        this.mapTypes.add(mapType);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder format(Integer format) {
      this.format = format;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder generatedFlag(Boolean generatedFlag) {
      this.generatedFlag = generatedFlag;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder maxLength(Integer maxLength) {
      this.maxLength = maxLength;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder uniqueItems(Boolean uniqueItems) {
      this.uniqueItems = uniqueItems;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder elements(Integer elements) {
      this.elements = elements;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder regex(String regex) {
      this.regex = regex;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder minItems(Integer minItems) {
      this.minItems = minItems;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder values(List<ApiTypeArrayDTO> values) {
      if (!values.isEmpty()) {
        this.values.addAll(values);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder value(ApiTypeArrayDTO value) {
      if (value != null) {
        this.values.add(value);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder minimum(Integer minimum) {
      this.minimum = minimum;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder numberEnum(NumberEnum numberEnum) {
      this.numberEnum = numberEnum;
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder valueLength(Integer valueLength) {
      this.valueLength = valueLength;
      return this;
    }

    public ApiTypeArrayDTO build() {
      ApiTypeArrayDTO apiTypeArrayDTO = new ApiTypeArrayDTO(this);
      return apiTypeArrayDTO;
    }
  }

  @Schema(name = "precision", required = false)
  public Integer getPrecision() {
    return precision;
  }
  public void setPrecision(Integer precision) {
    this.precision = precision;
  }

  @Schema(name = "defaultItem", required = false)
  public ApiDefaultItemDTO getDefaultItem() {
    return defaultItem;
  }
  public void setDefaultItem(ApiDefaultItemDTO defaultItem) {
    this.defaultItem = defaultItem;
  }

  @Schema(name = "maximum", required = false)
  public Integer getMaximum() {
    return maximum;
  }
  public void setMaximum(Integer maximum) {
    this.maximum = maximum;
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "scale", required = false)
  public Integer getScale() {
    return scale;
  }
  public void setScale(Integer scale) {
    this.scale = scale;
  }

  @Schema(name = "minLength", required = false)
  public Integer getMinLength() {
    return minLength;
  }
  public void setMinLength(Integer minLength) {
    this.minLength = minLength;
  }

  @Schema(name = "increment", required = false)
  public Integer getIncrement() {
    return increment;
  }
  public void setIncrement(Integer increment) {
    this.increment = increment;
  }

  @Schema(name = "keyType", required = false)
  public String getKeyType() {
    return keyType;
  }
  public void setKeyType(String keyType) {
    this.keyType = keyType;
  }

  @Schema(name = "requiredValues", required = false)
  public List<String> getRequiredValues() {
    return requiredValues;
  }
  public void setRequiredValues(List<String> requiredValues) {
    this.requiredValues = requiredValues;
  }

  @Schema(name = "optionalUnion", required = false)
  public Boolean getOptionalUnion() {
    return optionalUnion;
  }
  public void setOptionalUnion(Boolean optionalUnion) {
    this.optionalUnion = optionalUnion;
  }

  @Schema(name = "initialValue", required = false)
  public String getInitialValue() {
    return initialValue;
  }
  public void setInitialValue(String initialValue) {
    this.initialValue = initialValue;
  }

  @Schema(name = "flagExclusiveMinimum", required = false)
  public Boolean getFlagExclusiveMinimum() {
    return flagExclusiveMinimum;
  }
  public void setFlagExclusiveMinimum(Boolean flagExclusiveMinimum) {
    this.flagExclusiveMinimum = flagExclusiveMinimum;
  }

  @Schema(name = "flagExclusiveMaximum", required = false)
  public Boolean getFlagExclusiveMaximum() {
    return flagExclusiveMaximum;
  }
  public void setFlagExclusiveMaximum(Boolean flagExclusiveMaximum) {
    this.flagExclusiveMaximum = flagExclusiveMaximum;
  }

  @Schema(name = "defaultValue", required = false)
  public Boolean getDefaultValue() {
    return defaultValue;
  }
  public void setDefaultValue(Boolean defaultValue) {
    this.defaultValue = defaultValue;
  }

  @Schema(name = "enumValues", required = false)
  public List<String> getEnumValues() {
    return enumValues;
  }
  public void setEnumValues(List<String> enumValues) {
    this.enumValues = enumValues;
  }

  @Schema(name = "properties", required = false)
  public List<String> getProperties() {
    return properties;
  }
  public void setProperties(List<String> properties) {
    this.properties = properties;
  }

  @Schema(name = "defaultValues", required = false)
  public List<String> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<String> defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "unionEnum", required = false)
  public UnionEnum getUnionEnum() {
    return unionEnum;
  }
  public void setUnionEnum(UnionEnum unionEnum) {
    this.unionEnum = unionEnum;
  }

  @Schema(name = "seqEnum", required = false)
  public SeqEnum getSeqEnum() {
    return seqEnum;
  }
  public void setSeqEnum(SeqEnum seqEnum) {
    this.seqEnum = seqEnum;
  }

  @Schema(name = "arraySize", required = false)
  public Integer getArraySize() {
    return arraySize;
  }
  public void setArraySize(Integer arraySize) {
    this.arraySize = arraySize;
  }

  @Schema(name = "multipleOf", required = false)
  public Integer getMultipleOf() {
    return multipleOf;
  }
  public void setMultipleOf(Integer multipleOf) {
    this.multipleOf = multipleOf;
  }

  @Schema(name = "mapSize", required = false)
  public Integer getMapSize() {
    return mapSize;
  }
  public void setMapSize(Integer mapSize) {
    this.mapSize = mapSize;
  }

  @Schema(name = "mapTypes", required = false)
  public List<ApiTypeArrayDTO> getMapTypes() {
    return mapTypes;
  }
  public void setMapTypes(List<ApiTypeArrayDTO> mapTypes) {
    this.mapTypes = mapTypes;
  }

  @Schema(name = "format", required = false)
  public Integer getFormat() {
    return format;
  }
  public void setFormat(Integer format) {
    this.format = format;
  }

  @Schema(name = "generatedFlag", required = false)
  public Boolean getGeneratedFlag() {
    return generatedFlag;
  }
  public void setGeneratedFlag(Boolean generatedFlag) {
    this.generatedFlag = generatedFlag;
  }

  @Schema(name = "maxLength", required = false)
  public Integer getMaxLength() {
    return maxLength;
  }
  public void setMaxLength(Integer maxLength) {
    this.maxLength = maxLength;
  }

  @Schema(name = "uniqueItems", required = false)
  public Boolean getUniqueItems() {
    return uniqueItems;
  }
  public void setUniqueItems(Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
  }

  @Schema(name = "elements", required = false)
  public Integer getElements() {
    return elements;
  }
  public void setElements(Integer elements) {
    this.elements = elements;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "regex", required = false)
  public String getRegex() {
    return regex;
  }
  public void setRegex(String regex) {
    this.regex = regex;
  }

  @Schema(name = "minItems", required = false)
  public Integer getMinItems() {
    return minItems;
  }
  public void setMinItems(Integer minItems) {
    this.minItems = minItems;
  }

  @Schema(name = "values", required = false)
  public List<ApiTypeArrayDTO> getValues() {
    return values;
  }
  public void setValues(List<ApiTypeArrayDTO> values) {
    this.values = values;
  }

  @Schema(name = "minimum", required = false)
  public Integer getMinimum() {
    return minimum;
  }
  public void setMinimum(Integer minimum) {
    this.minimum = minimum;
  }

  @Schema(name = "numberEnum", required = false)
  public NumberEnum getNumberEnum() {
    return numberEnum;
  }
  public void setNumberEnum(NumberEnum numberEnum) {
    this.numberEnum = numberEnum;
  }

  @Schema(name = "valueLength", required = false)
  public Integer getValueLength() {
    return valueLength;
  }
  public void setValueLength(Integer valueLength) {
    this.valueLength = valueLength;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTypeArrayDTO apiTypeArrayDTO = (ApiTypeArrayDTO) o;
    return Objects.equals(this.precision, apiTypeArrayDTO.precision) && Objects.equals(this.defaultItem, apiTypeArrayDTO.defaultItem) && Objects.equals(this.maximum, apiTypeArrayDTO.maximum) && Objects.equals(this.type, apiTypeArrayDTO.type) && Objects.equals(this.scale, apiTypeArrayDTO.scale) && Objects.equals(this.minLength, apiTypeArrayDTO.minLength) && Objects.equals(this.increment, apiTypeArrayDTO.increment) && Objects.equals(this.keyType, apiTypeArrayDTO.keyType) && Objects.equals(this.requiredValues, apiTypeArrayDTO.requiredValues) && Objects.equals(this.optionalUnion, apiTypeArrayDTO.optionalUnion) && Objects.equals(this.initialValue, apiTypeArrayDTO.initialValue) && Objects.equals(this.flagExclusiveMinimum, apiTypeArrayDTO.flagExclusiveMinimum) && Objects.equals(this.flagExclusiveMaximum, apiTypeArrayDTO.flagExclusiveMaximum) && Objects.equals(this.defaultValue, apiTypeArrayDTO.defaultValue) && Objects.equals(this.enumValues, apiTypeArrayDTO.enumValues) && Objects.equals(this.properties, apiTypeArrayDTO.properties) && Objects.equals(this.defaultValues, apiTypeArrayDTO.defaultValues) && Objects.equals(this.unionEnum, apiTypeArrayDTO.unionEnum) && Objects.equals(this.seqEnum, apiTypeArrayDTO.seqEnum) && Objects.equals(this.arraySize, apiTypeArrayDTO.arraySize) && Objects.equals(this.multipleOf, apiTypeArrayDTO.multipleOf) && Objects.equals(this.mapSize, apiTypeArrayDTO.mapSize) && Objects.equals(this.mapTypes, apiTypeArrayDTO.mapTypes) && Objects.equals(this.format, apiTypeArrayDTO.format) && Objects.equals(this.generatedFlag, apiTypeArrayDTO.generatedFlag) && Objects.equals(this.maxLength, apiTypeArrayDTO.maxLength) && Objects.equals(this.uniqueItems, apiTypeArrayDTO.uniqueItems) && Objects.equals(this.elements, apiTypeArrayDTO.elements) && Objects.equals(this.name, apiTypeArrayDTO.name) && Objects.equals(this.regex, apiTypeArrayDTO.regex) && Objects.equals(this.minItems, apiTypeArrayDTO.minItems) && Objects.equals(this.values, apiTypeArrayDTO.values) && Objects.equals(this.minimum, apiTypeArrayDTO.minimum) && Objects.equals(this.numberEnum, apiTypeArrayDTO.numberEnum) && Objects.equals(this.valueLength, apiTypeArrayDTO.valueLength);
  }

  @Override
  public int hashCode() {
    return Objects.hash(precision, defaultItem, maximum, type, scale, minLength, increment, keyType, requiredValues, optionalUnion, initialValue, flagExclusiveMinimum, flagExclusiveMaximum, defaultValue, enumValues, properties, defaultValues, unionEnum, seqEnum, arraySize, multipleOf, mapSize, mapTypes, format, generatedFlag, maxLength, uniqueItems, elements, name, regex, minItems, values, minimum, numberEnum, valueLength);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTypeArrayDTO{");
    sb.append(" precision:").append(precision).append(",");
    sb.append(" defaultItem:").append(defaultItem).append(",");
    sb.append(" maximum:").append(maximum).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" scale:").append(scale).append(",");
    sb.append(" minLength:").append(minLength).append(",");
    sb.append(" increment:").append(increment).append(",");
    sb.append(" keyType:").append(keyType).append(",");
    sb.append(" requiredValues:").append(requiredValues).append(",");
    sb.append(" optionalUnion:").append(optionalUnion).append(",");
    sb.append(" initialValue:").append(initialValue).append(",");
    sb.append(" flagExclusiveMinimum:").append(flagExclusiveMinimum).append(",");
    sb.append(" flagExclusiveMaximum:").append(flagExclusiveMaximum).append(",");
    sb.append(" defaultValue:").append(defaultValue).append(",");
    sb.append(" enumValues:").append(enumValues).append(",");
    sb.append(" properties:").append(properties).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" unionEnum:").append(unionEnum).append(",");
    sb.append(" seqEnum:").append(seqEnum).append(",");
    sb.append(" arraySize:").append(arraySize).append(",");
    sb.append(" multipleOf:").append(multipleOf).append(",");
    sb.append(" mapSize:").append(mapSize).append(",");
    sb.append(" mapTypes:").append(mapTypes).append(",");
    sb.append(" format:").append(format).append(",");
    sb.append(" generatedFlag:").append(generatedFlag).append(",");
    sb.append(" maxLength:").append(maxLength).append(",");
    sb.append(" uniqueItems:").append(uniqueItems).append(",");
    sb.append(" elements:").append(elements).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" regex:").append(regex).append(",");
    sb.append(" minItems:").append(minItems).append(",");
    sb.append(" values:").append(values).append(",");
    sb.append(" minimum:").append(minimum).append(",");
    sb.append(" numberEnum:").append(numberEnum).append(",");
    sb.append(" valueLength:").append(valueLength);
    sb.append("}");
    return sb.toString();
  }

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.precision)) {
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
    } else if (Objects.nonNull(this.enumValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.properties)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.defaultValues)) {
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
      throw new ModelClassException("ApiTypeArrayDTO");
    }
  }

}
