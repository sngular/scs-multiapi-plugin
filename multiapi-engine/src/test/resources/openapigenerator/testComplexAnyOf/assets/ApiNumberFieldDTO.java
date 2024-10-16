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
  private List<Object> defaultValues;
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

    public ApiNumberFieldDTO.ApiNumberFieldDTOBuilder defaultValue(Object defaultValue) {
      if (Objects.nonNull(defaultValue)) {
        this.defaultValues.add(defaultValue);
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

  @Schema(name = "precision", required = false)
  public Integer getPrecision() {
    return precision;
  }
  public void setPrecision(Integer precision) {
    this.precision = precision;
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

  @Schema(name = "defaultValues", required = false)
  public List<Object> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<Object> defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "multipleOf", required = false)
  public Integer getMultipleOf() {
    return multipleOf;
  }
  public void setMultipleOf(Integer multipleOf) {
    this.multipleOf = multipleOf;
  }

  @Schema(name = "scale", required = false)
  public Integer getScale() {
    return scale;
  }
  public void setScale(Integer scale) {
    this.scale = scale;
  }

  @Schema(name = "minimum", required = false)
  public Integer getMinimum() {
    return minimum;
  }
  public void setMinimum(Integer minimum) {
    this.minimum = minimum;
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

  @Schema(name = "numberEnum", required = false)
  public NumberEnum getNumberEnum() {
    return numberEnum;
  }
  public void setNumberEnum(NumberEnum numberEnum) {
    this.numberEnum = numberEnum;
  }

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
    sb.append("ApiNumberFieldDTO{");
    sb.append(" precision:").append(precision).append(",");
    sb.append(" maximum:").append(maximum).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" multipleOf:").append(multipleOf).append(",");
    sb.append(" scale:").append(scale).append(",");
    sb.append(" minimum:").append(minimum).append(",");
    sb.append(" flagExclusiveMinimum:").append(flagExclusiveMinimum).append(",");
    sb.append(" flagExclusiveMaximum:").append(flagExclusiveMaximum).append(",");
    sb.append(" numberEnum:").append(numberEnum).append(",");
    sb.append(" defaultValue:").append(defaultValue);
    sb.append("}");
    return sb.toString();
  }


}
