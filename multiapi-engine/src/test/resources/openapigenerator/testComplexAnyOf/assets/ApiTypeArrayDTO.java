package net.coru.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import net.coru.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

public class ApiTypeArrayDTO {

  @JsonProperty(value ="precision")
  private Integer precision;
  @JsonProperty(value ="defaultItem")
  private Object defaultItem;
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
  private List<Object> requiredValues = new ArrayList<Object>();
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
  private List<Object> enumValues = new ArrayList<Object>();
  @JsonProperty(value ="properties")
  private List<Object> properties = new ArrayList<Object>();
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
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

  private ApiTypeArrayDTO(Integer precision, Object defaultItem, Integer maximum, String type, Integer scale, Integer minLength, Integer increment, String keyType, List<Object> requiredValues, Boolean optionalUnion, String initialValue, Boolean flagExclusiveMinimum, Boolean flagExclusiveMaximum, Boolean defaultValue, List<Object> enumValues, List<Object> properties, List<Object> defaultValues, UnionEnum unionEnum, SeqEnum seqEnum, Integer arraySize, Integer multipleOf, Integer mapSize, List<ApiTypeArrayDTO> mapTypes, Integer format, Boolean generatedFlag, Integer maxLength, Boolean uniqueItems, Integer elements, String name, String regex, Integer minItems, List<ApiTypeArrayDTO> values, Integer minimum, NumberEnum numberEnum, Integer valueLength) {
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

  public static class ApiTypeArrayDTOBuilder {

    private Integer precision;
    private Object defaultItem;
    private Integer maximum;
    private String type;
    private Integer scale;
    private Integer minLength;
    private Integer increment;
    private String keyType;
    private List<Object> requiredValues = new ArrayList<Object>();
    private Boolean optionalUnion;
    private String initialValue;
    private Boolean flagExclusiveMinimum;
    private Boolean flagExclusiveMaximum;
    private Boolean defaultValue;
    private List<Object> enumValues = new ArrayList<Object>();
    private List<Object> properties = new ArrayList<Object>();
    private List<Object> defaultValues = new ArrayList<Object>();
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

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultItem(Object defaultItem) {
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
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder requiredValues(List<Object> requiredValues) {
      if (!requiredValues.isEmpty()) {
        this.requiredValues.addAll(requiredValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder requiredValue(Object requiredValue) {
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
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder enumValues(List<Object> enumValues) {
      if (!enumValues.isEmpty()) {
        this.enumValues.addAll(enumValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder enumValue(Object enumValue) {
      if (enumValue != null) {
        this.enumValues.add(enumValue);
      }
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder properties(List<Object> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder propertie(Object propertie) {
      if (propertie != null) {
        this.properties.add(propertie);
      }
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder defaultValue(Object defaultValue) {
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

  /**
  * Get precision
  * @return precision
  */
  @Schema(name = "precision", required = false)
  public Integer getPrecision() {
    return precision;
  }
  public void setPrecision(Integer precision) {
    this.precision = precision;
  }

  /**
  * Get defaultItem
  * @return defaultItem
  */
  @Schema(name = "defaultItem", required = false)
  public Object getDefaultItem() {
    return defaultItem;
  }
  public void setDefaultItem(Object defaultItem) {
    this.defaultItem = defaultItem;
  }

  /**
  * Get maximum
  * @return maximum
  */
  @Schema(name = "maximum", required = false)
  public Integer getMaximum() {
    return maximum;
  }
  public void setMaximum(Integer maximum) {
    this.maximum = maximum;
  }

  /**
  * Get type
  * @return type
  */
  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  /**
  * Get scale
  * @return scale
  */
  @Schema(name = "scale", required = false)
  public Integer getScale() {
    return scale;
  }
  public void setScale(Integer scale) {
    this.scale = scale;
  }

  /**
  * Get minLength
  * @return minLength
  */
  @Schema(name = "minLength", required = false)
  public Integer getMinLength() {
    return minLength;
  }
  public void setMinLength(Integer minLength) {
    this.minLength = minLength;
  }

  /**
  * Get increment
  * @return increment
  */
  @Schema(name = "increment", required = false)
  public Integer getIncrement() {
    return increment;
  }
  public void setIncrement(Integer increment) {
    this.increment = increment;
  }

  /**
  * Get keyType
  * @return keyType
  */
  @Schema(name = "keyType", required = false)
  public String getKeyType() {
    return keyType;
  }
  public void setKeyType(String keyType) {
    this.keyType = keyType;
  }

  /**
  * Get requiredValues
  * @return requiredValues
  */
  @Schema(name = "requiredValues", required = false)
  public List<Object> getRequiredValues() {
    return requiredValues;
  }
  public void setRequiredValues(List<Object> requiredValues) {
    this.requiredValues = requiredValues;
  }

  /**
  * Get optionalUnion
  * @return optionalUnion
  */
  @Schema(name = "optionalUnion", required = false)
  public Boolean getOptionalUnion() {
    return optionalUnion;
  }
  public void setOptionalUnion(Boolean optionalUnion) {
    this.optionalUnion = optionalUnion;
  }

  /**
  * Get initialValue
  * @return initialValue
  */
  @Schema(name = "initialValue", required = false)
  public String getInitialValue() {
    return initialValue;
  }
  public void setInitialValue(String initialValue) {
    this.initialValue = initialValue;
  }

  /**
  * Get flagExclusiveMinimum
  * @return flagExclusiveMinimum
  */
  @Schema(name = "flagExclusiveMinimum", required = false)
  public Boolean getFlagExclusiveMinimum() {
    return flagExclusiveMinimum;
  }
  public void setFlagExclusiveMinimum(Boolean flagExclusiveMinimum) {
    this.flagExclusiveMinimum = flagExclusiveMinimum;
  }

  /**
  * Get flagExclusiveMaximum
  * @return flagExclusiveMaximum
  */
  @Schema(name = "flagExclusiveMaximum", required = false)
  public Boolean getFlagExclusiveMaximum() {
    return flagExclusiveMaximum;
  }
  public void setFlagExclusiveMaximum(Boolean flagExclusiveMaximum) {
    this.flagExclusiveMaximum = flagExclusiveMaximum;
  }

  /**
  * Get defaultValue
  * @return defaultValue
  */
  @Schema(name = "defaultValue", required = false)
  public Boolean getDefaultValue() {
    return defaultValue;
  }
  public void setDefaultValue(Boolean defaultValue) {
    this.defaultValue = defaultValue;
  }

  /**
  * Get enumValues
  * @return enumValues
  */
  @Schema(name = "enumValues", required = false)
  public List<Object> getEnumValues() {
    return enumValues;
  }
  public void setEnumValues(List<Object> enumValues) {
    this.enumValues = enumValues;
  }

  /**
  * Get properties
  * @return properties
  */
  @Schema(name = "properties", required = false)
  public List<Object> getProperties() {
    return properties;
  }
  public void setProperties(List<Object> properties) {
    this.properties = properties;
  }

  /**
  * Get defaultValues
  * @return defaultValues
  */
  @Schema(name = "defaultValues", required = false)
  public List<Object> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<Object> defaultValues) {
    this.defaultValues = defaultValues;
  }

  /**
  * Get unionEnum
  * @return unionEnum
  */
  @Schema(name = "unionEnum", required = false)
  public UnionEnum getUnionEnum() {
    return unionEnum;
  }
  public void setUnionEnum(UnionEnum unionEnum) {
    this.unionEnum = unionEnum;
  }

  /**
  * Get seqEnum
  * @return seqEnum
  */
  @Schema(name = "seqEnum", required = false)
  public SeqEnum getSeqEnum() {
    return seqEnum;
  }
  public void setSeqEnum(SeqEnum seqEnum) {
    this.seqEnum = seqEnum;
  }

  /**
  * Get arraySize
  * @return arraySize
  */
  @Schema(name = "arraySize", required = false)
  public Integer getArraySize() {
    return arraySize;
  }
  public void setArraySize(Integer arraySize) {
    this.arraySize = arraySize;
  }

  /**
  * Get multipleOf
  * @return multipleOf
  */
  @Schema(name = "multipleOf", required = false)
  public Integer getMultipleOf() {
    return multipleOf;
  }
  public void setMultipleOf(Integer multipleOf) {
    this.multipleOf = multipleOf;
  }

  /**
  * Get mapSize
  * @return mapSize
  */
  @Schema(name = "mapSize", required = false)
  public Integer getMapSize() {
    return mapSize;
  }
  public void setMapSize(Integer mapSize) {
    this.mapSize = mapSize;
  }

  /**
  * Get mapTypes
  * @return mapTypes
  */
  @Schema(name = "mapTypes", required = false)
  public List<ApiTypeArrayDTO> getMapTypes() {
    return mapTypes;
  }
  public void setMapTypes(List<ApiTypeArrayDTO> mapTypes) {
    this.mapTypes = mapTypes;
  }

  /**
  * Get format
  * @return format
  */
  @Schema(name = "format", required = false)
  public Integer getFormat() {
    return format;
  }
  public void setFormat(Integer format) {
    this.format = format;
  }

  /**
  * Get generatedFlag
  * @return generatedFlag
  */
  @Schema(name = "generatedFlag", required = false)
  public Boolean getGeneratedFlag() {
    return generatedFlag;
  }
  public void setGeneratedFlag(Boolean generatedFlag) {
    this.generatedFlag = generatedFlag;
  }

  /**
  * Get maxLength
  * @return maxLength
  */
  @Schema(name = "maxLength", required = false)
  public Integer getMaxLength() {
    return maxLength;
  }
  public void setMaxLength(Integer maxLength) {
    this.maxLength = maxLength;
  }

  /**
  * Get uniqueItems
  * @return uniqueItems
  */
  @Schema(name = "uniqueItems", required = false)
  public Boolean getUniqueItems() {
    return uniqueItems;
  }
  public void setUniqueItems(Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
  }

  /**
  * Get elements
  * @return elements
  */
  @Schema(name = "elements", required = false)
  public Integer getElements() {
    return elements;
  }
  public void setElements(Integer elements) {
    this.elements = elements;
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
  * Get regex
  * @return regex
  */
  @Schema(name = "regex", required = false)
  public String getRegex() {
    return regex;
  }
  public void setRegex(String regex) {
    this.regex = regex;
  }

  /**
  * Get minItems
  * @return minItems
  */
  @Schema(name = "minItems", required = false)
  public Integer getMinItems() {
    return minItems;
  }
  public void setMinItems(Integer minItems) {
    this.minItems = minItems;
  }

  /**
  * Get values
  * @return values
  */
  @Schema(name = "values", required = false)
  public List<ApiTypeArrayDTO> getValues() {
    return values;
  }
  public void setValues(List<ApiTypeArrayDTO> values) {
    this.values = values;
  }

  /**
  * Get minimum
  * @return minimum
  */
  @Schema(name = "minimum", required = false)
  public Integer getMinimum() {
    return minimum;
  }
  public void setMinimum(Integer minimum) {
    this.minimum = minimum;
  }

  /**
  * Get numberEnum
  * @return numberEnum
  */
  @Schema(name = "numberEnum", required = false)
  public NumberEnum getNumberEnum() {
    return numberEnum;
  }
  public void setNumberEnum(NumberEnum numberEnum) {
    this.numberEnum = numberEnum;
  }

  /**
  * Get valueLength
  * @return valueLength
  */
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
    sb.append("class ApiTypeArrayDTO {\n");
    sb.append(" precision: ").append(toIndentedString(precision)).append("\n");
    sb.append(" defaultItem: ").append(toIndentedString(defaultItem)).append("\n");
    sb.append(" maximum: ").append(toIndentedString(maximum)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" scale: ").append(toIndentedString(scale)).append("\n");
    sb.append(" minLength: ").append(toIndentedString(minLength)).append("\n");
    sb.append(" increment: ").append(toIndentedString(increment)).append("\n");
    sb.append(" keyType: ").append(toIndentedString(keyType)).append("\n");
    sb.append(" requiredValues: ").append(toIndentedString(requiredValues)).append("\n");
    sb.append(" optionalUnion: ").append(toIndentedString(optionalUnion)).append("\n");
    sb.append(" initialValue: ").append(toIndentedString(initialValue)).append("\n");
    sb.append(" flagExclusiveMinimum: ").append(toIndentedString(flagExclusiveMinimum)).append("\n");
    sb.append(" flagExclusiveMaximum: ").append(toIndentedString(flagExclusiveMaximum)).append("\n");
    sb.append(" defaultValue: ").append(toIndentedString(defaultValue)).append("\n");
    sb.append(" enumValues: ").append(toIndentedString(enumValues)).append("\n");
    sb.append(" properties: ").append(toIndentedString(properties)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" unionEnum: ").append(toIndentedString(unionEnum)).append("\n");
    sb.append(" seqEnum: ").append(toIndentedString(seqEnum)).append("\n");
    sb.append(" arraySize: ").append(toIndentedString(arraySize)).append("\n");
    sb.append(" multipleOf: ").append(toIndentedString(multipleOf)).append("\n");
    sb.append(" mapSize: ").append(toIndentedString(mapSize)).append("\n");
    sb.append(" mapTypes: ").append(toIndentedString(mapTypes)).append("\n");
    sb.append(" format: ").append(toIndentedString(format)).append("\n");
    sb.append(" generatedFlag: ").append(toIndentedString(generatedFlag)).append("\n");
    sb.append(" maxLength: ").append(toIndentedString(maxLength)).append("\n");
    sb.append(" uniqueItems: ").append(toIndentedString(uniqueItems)).append("\n");
    sb.append(" elements: ").append(toIndentedString(elements)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" regex: ").append(toIndentedString(regex)).append("\n");
    sb.append(" minItems: ").append(toIndentedString(minItems)).append("\n");
    sb.append(" values: ").append(toIndentedString(values)).append("\n");
    sb.append(" minimum: ").append(toIndentedString(minimum)).append("\n");
    sb.append(" numberEnum: ").append(toIndentedString(numberEnum)).append("\n");
    sb.append(" valueLength: ").append(toIndentedString(valueLength)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
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
