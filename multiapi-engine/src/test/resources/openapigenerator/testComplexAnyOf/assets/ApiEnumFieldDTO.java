package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

public class ApiEnumFieldDTO {

  @JsonProperty(value ="enumValues")
  private List<String> enumValues = new ArrayList<String>();
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private List<String> defaultValues = new ArrayList<String>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="defaultValue")
  private String defaultValue;

  private ApiEnumFieldDTO(List<String> enumValues, String type, List<String> defaultValues, String name, String defaultValue) {
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

    private List<String> enumValues = new ArrayList<String>();
    private String type;
    private List<String> defaultValues = new ArrayList<String>();
    private String name;
    private String defaultValue;
    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder enumValues(List<String> enumValues) {
      if (!enumValues.isEmpty()) {
        this.enumValues.addAll(enumValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder enumValues(String enumValues) {
      if (enumValues != null) {
        this.enumValues.add(enumValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder defaultValues(List<String> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiEnumFieldDTO.ApiEnumFieldDTOBuilder defaultValues(String defaultValues) {
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
  public List<String> getEnumValues() {
    return enumValues;
  }
  public void setEnumValues(List<String> enumValues) {
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
  public List<String> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<String> defaultValues) {
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
