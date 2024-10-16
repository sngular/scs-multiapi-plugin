package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder.class)
public class ApiSequenceFieldDTO {

  @JsonProperty(value ="elements")
  private Integer elements;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="properties")
  private List<ApiTypeArrayDTO> properties;
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues;
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
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="initialValue")
  private String initialValue;
  @JsonProperty(value ="increment")
  private Integer increment;

  private ApiSequenceFieldDTO(ApiSequenceFieldDTOBuilder builder) {
    this.elements = builder.elements;
    this.type = builder.type;
    this.properties = builder.properties;
    this.defaultValues = builder.defaultValues;
    this.seqEnum = builder.seqEnum;
    this.name = builder.name;
    this.initialValue = builder.initialValue;
    this.increment = builder.increment;

  }

  public static ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder builder() {
    return new ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiSequenceFieldDTOBuilder {

    private Integer elements;
    private String type;
    private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
    private List<Object> defaultValues = new ArrayList<Object>();
    private SeqEnum seqEnum;
    private String name;
    private String initialValue;
    private Integer increment;

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder elements(Integer elements) {
      this.elements = elements;
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder properties(List<ApiTypeArrayDTO> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder property(ApiTypeArrayDTO property) {
      if (Objects.nonNull(property)) {
        this.properties.add(property);
      }
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder defaultValue(Object defaultValue) {
      if (Objects.nonNull(defaultValue)) {
        this.defaultValues.add(defaultValue);
      }
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder seqEnum(SeqEnum seqEnum) {
      this.seqEnum = seqEnum;
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder initialValue(String initialValue) {
      this.initialValue = initialValue;
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder increment(Integer increment) {
      this.increment = increment;
      return this;
    }

    public ApiSequenceFieldDTO build() {
      ApiSequenceFieldDTO apiSequenceFieldDTO = new ApiSequenceFieldDTO(this);
      return apiSequenceFieldDTO;
    }
  }

  @Schema(name = "elements", required = false)
  public Integer getElements() {
    return elements;
  }
  public void setElements(Integer elements) {
    this.elements = elements;
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "properties", required = false)
  public List<ApiTypeArrayDTO> getProperties() {
    return properties;
  }
  public void setProperties(List<ApiTypeArrayDTO> properties) {
    this.properties = properties;
  }

  @Schema(name = "defaultValues", required = false)
  public List<Object> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<Object> defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "seqEnum", required = false)
  public SeqEnum getSeqEnum() {
    return seqEnum;
  }
  public void setSeqEnum(SeqEnum seqEnum) {
    this.seqEnum = seqEnum;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "initialValue", required = false)
  public String getInitialValue() {
    return initialValue;
  }
  public void setInitialValue(String initialValue) {
    this.initialValue = initialValue;
  }

  @Schema(name = "increment", required = false)
  public Integer getIncrement() {
    return increment;
  }
  public void setIncrement(Integer increment) {
    this.increment = increment;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiSequenceFieldDTO apiSequenceFieldDTO = (ApiSequenceFieldDTO) o;
    return Objects.equals(this.elements, apiSequenceFieldDTO.elements) && Objects.equals(this.type, apiSequenceFieldDTO.type) && Objects.equals(this.properties, apiSequenceFieldDTO.properties) && Objects.equals(this.defaultValues, apiSequenceFieldDTO.defaultValues) && Objects.equals(this.seqEnum, apiSequenceFieldDTO.seqEnum) && Objects.equals(this.name, apiSequenceFieldDTO.name) && Objects.equals(this.initialValue, apiSequenceFieldDTO.initialValue) && Objects.equals(this.increment, apiSequenceFieldDTO.increment);
  }

  @Override
  public int hashCode() {
    return Objects.hash(elements, type, properties, defaultValues, seqEnum, name, initialValue, increment);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiSequenceFieldDTO{");
    sb.append(" elements:").append(elements).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" properties:").append(properties).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" seqEnum:").append(seqEnum).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" initialValue:").append(initialValue).append(",");
    sb.append(" increment:").append(increment);
    sb.append("}");
    return sb.toString();
  }


}
