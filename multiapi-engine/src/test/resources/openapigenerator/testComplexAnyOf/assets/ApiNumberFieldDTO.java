package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiNumberFieldDTO.ApiNumberFieldDTOBuilder.class)
public class ApiNumberFieldDTO {

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

  private ApiNumberFieldDTO(Integer precision, Integer maximum, String type, List<Object> defaultValues, String name, Integer multipleOf, Integer scale, Integer minimum, Boolean flagExclusiveMinimum, Boolean flagExclusiveMaximum, NumberEnum numberEnum, Long defaultValue) {
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

  private ApiNumberFieldDTO(ApiNumberFieldDTOBuilder builder) {
    this.precision = builder.precision;
    this.maximum = builder.maximum;
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.multipleOf = builder.multipleOf;
    this.scale = builder.scale;
    this.minimum = builder.minimum;
    this.flagExclusiveMinimum = builder.flagExclusiveMinimum;
    this.flagExclusiveMaximum = builder.flagExclusiveMaximum;
    this.numberEnum = builder.numberEnum;
    this.defaultValue = builder.defaultValue;

  }

  public static ApiNumberFieldDTO.ApiNumberFieldDTOBuilder builder() {
    return new ApiNumberFieldDTO.ApiNumberFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiNumberFieldDTOBuilder {

    private Integer precision;
    private Integer maximum;
    private String type;
    private List<Object> defaultValues = new ArrayList<Object>();
    private String name;
    private Integer multipleOf;
    private Integer scale;
    private Integer minimum;
    private Boolean flagExclusiveMinimum;
    private Boolean flagExclusiveMaximum;
    private NumberEnum numberEnum;
    private Long defaultValue;

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder precision(Integer precision) {
      this.precision = precision;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder maximum(Integer maximum) {
      this.maximum = maximum;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder defaultValues(Object defaultValues) {
      if (defaultValues != null) {
        this.defaultValues.add(defaultValues);
      }
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder multipleOf(Integer multipleOf) {
      this.multipleOf = multipleOf;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder scale(Integer scale) {
      this.scale = scale;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder minimum(Integer minimum) {
      this.minimum = minimum;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder flagExclusiveMinimum(Boolean flagExclusiveMinimum) {
      this.flagExclusiveMinimum = flagExclusiveMinimum;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder flagExclusiveMaximum(Boolean flagExclusiveMaximum) {
      this.flagExclusiveMaximum = flagExclusiveMaximum;
      return this;
    }
    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder numberEnum(NumberEnum numberEnum) {
      this.numberEnum = numberEnum;
      return this;
    }

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder defaultValue(Long defaultValue) {
      this.defaultValue = defaultValue;
      return this;
    }

    public ApiNumberFieldDTO build() {
      ApiNumberFieldDTO apiNumberFieldDTO = new ApiNumberFieldDTO(this);
      return apiNumberFieldDTO;
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
  * Get defaultValue
  * @return defaultValue
  */
  @Schema(name = "defaultValue", required = false)
  public Long getDefaultValue() {
    return defaultValue;
  }
  public void setDefaultValue(Long defaultValue) {
    this.defaultValue = defaultValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiNumberFieldDTO apiNumberFieldDTO = (ApiNumberFieldDTO) o;
    return Objects.equals(this.precision, apiNumberFieldDTO.precision) && Objects.equals(this.maximum, apiNumberFieldDTO.maximum) && Objects.equals(this.type, apiNumberFieldDTO.type) && Objects.equals(this.defaultValues, apiNumberFieldDTO.defaultValues) && Objects.equals(this.name, apiNumberFieldDTO.name) && Objects.equals(this.multipleOf, apiNumberFieldDTO.multipleOf) && Objects.equals(this.scale, apiNumberFieldDTO.scale) && Objects.equals(this.minimum, apiNumberFieldDTO.minimum) && Objects.equals(this.flagExclusiveMinimum, apiNumberFieldDTO.flagExclusiveMinimum) && Objects.equals(this.flagExclusiveMaximum, apiNumberFieldDTO.flagExclusiveMaximum) && Objects.equals(this.numberEnum, apiNumberFieldDTO.numberEnum) && Objects.equals(this.defaultValue, apiNumberFieldDTO.defaultValue);
  }

  @Override
  public int hashCode() {
    return Objects.hash(precision, maximum, type, defaultValues, name, multipleOf, scale, minimum, flagExclusiveMinimum, flagExclusiveMaximum, numberEnum, defaultValue);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiNumberFieldDTO {\n");
    sb.append(" precision: ").append(toIndentedString(precision)).append("\n");
    sb.append(" maximum: ").append(toIndentedString(maximum)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" multipleOf: ").append(toIndentedString(multipleOf)).append("\n");
    sb.append(" scale: ").append(toIndentedString(scale)).append("\n");
    sb.append(" minimum: ").append(toIndentedString(minimum)).append("\n");
    sb.append(" flagExclusiveMinimum: ").append(toIndentedString(flagExclusiveMinimum)).append("\n");
    sb.append(" flagExclusiveMaximum: ").append(toIndentedString(flagExclusiveMaximum)).append("\n");
    sb.append(" numberEnum: ").append(toIndentedString(numberEnum)).append("\n");
    sb.append(" defaultValue: ").append(toIndentedString(defaultValue)).append("\n");
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



}
