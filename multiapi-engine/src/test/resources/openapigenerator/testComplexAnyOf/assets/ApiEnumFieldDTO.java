package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

public class ApiEnumFieldDTO {

  @JsonProperty(value ="enumValues")
  private List<Object> enumValues = new ArrayList<Object>();
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  private ApiEnumFieldDTO(List<Object> enumValues, String type, List<Object> defaultValues, String name, String defaultValue) {
    this.enumValues = enumValues;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.defaultValue = defaultValue;

  }

  private ApiEnumFieldDTO(ApiEnumFieldDTOBuilder builder) {
    this.enumValues = builder.enumValues;
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.defaultValue = builder.defaultValue;

  }

  public static ApiEnumFieldDTO.ApiEnumFieldDTOBuilder builder() {
    return new ApiEnumFieldDTO.ApiEnumFieldDTOBuilder();
  }

  public static class ApiEnumFieldDTOBuilder {

    private List<Object> enumValues = new ArrayList<Object>();
    private String type;
    private List<Object> defaultValues = new ArrayList<Object>();
    private String name;
    private String defaultValue;
    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder enumValues(List<Object> enumValues) {
      if (!enumValues.isEmpty()) {
        this.enumValues.addAll(enumValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder enumValues(Object enumValues) {
      if (enumValues != null) {
        this.enumValues.add(enumValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder defaultValues(Object defaultValues) {
      if (defaultValues != null) {
        this.defaultValues.add(defaultValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder defaultValue(String defaultValue) {
      this.defaultValue = defaultValue;
      return this;
    }

    public ApiEnumFieldDTO build() {
      ApiEnumFieldDTO apiEnumFieldDTO = new ApiEnumFieldDTO(this);
      return apiEnumFieldDTO;
    }
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
  * Get defaultValue
  * @return defaultValue
  */
  @Schema(name = "defaultValue", required = false)
  public String getDefaultValue() {
    return defaultValue;
  }
  public void setDefaultValue(String defaultValue) {
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
    ApiEnumFieldDTO apiEnumFieldDTO = (ApiEnumFieldDTO) o;
    return Objects.equals(this.enumValues, apiEnumFieldDTO.enumValues) && Objects.equals(this.type, apiEnumFieldDTO.type) && Objects.equals(this.defaultValues, apiEnumFieldDTO.defaultValues) && Objects.equals(this.name, apiEnumFieldDTO.name) && Objects.equals(this.defaultValue, apiEnumFieldDTO.defaultValue);
  }

  @Override
  public int hashCode() {
    return Objects.hash(enumValues, type, defaultValues, name, defaultValue);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiEnumFieldDTO {\n");
    sb.append(" enumValues: ").append(toIndentedString(enumValues)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
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
