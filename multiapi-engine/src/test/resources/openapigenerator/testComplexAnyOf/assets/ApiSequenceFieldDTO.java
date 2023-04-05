package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testcomplexanyof.model.ApiTypeArrayDTO;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder.class)
public class ApiSequenceFieldDTO {

  @JsonProperty(value ="elements")
  private Integer elements;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="properties")
  private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
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

  private ApiSequenceFieldDTO(Integer elements, String type, List<ApiTypeArrayDTO> properties, List<Object> defaultValues, SeqEnum seqEnum, String name, String initialValue, Integer increment) {
    this.elements = elements;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.seqEnum = seqEnum;
    this.name = name;
    this.initialValue = initialValue;
    this.increment = increment;

  }

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

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder properties(ApiTypeArrayDTO properties) {
      if (properties != null) {
        this.properties.add(properties);
      }
      return this;
    }
    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiSequenceFieldDTO.ApiSequenceFieldDTOBuilder defaultValues(Object defaultValues) {
      if (defaultValues != null) {
        this.defaultValues.add(defaultValues);
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
  * Get properties
  * @return properties
  */
  @Schema(name = "properties", required = false)
  public List<ApiTypeArrayDTO> getProperties() {
    return properties;
  }
  public void setProperties(List<ApiTypeArrayDTO> properties) {
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
    sb.append("class ApiSequenceFieldDTO {\n");
    sb.append(" elements: ").append(toIndentedString(elements)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" properties: ").append(toIndentedString(properties)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" seqEnum: ").append(toIndentedString(seqEnum)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" initialValue: ").append(toIndentedString(initialValue)).append("\n");
    sb.append(" increment: ").append(toIndentedString(increment)).append("\n");
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
