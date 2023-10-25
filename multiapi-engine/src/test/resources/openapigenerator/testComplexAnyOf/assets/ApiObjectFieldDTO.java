package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiObjectFieldDTO.ApiObjectFieldDTOBuilder.class)
public class ApiObjectFieldDTO {

  @JsonProperty(value ="requiredValues")
  private List<String> requiredValues = new ArrayList<String>();
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="properties")
  private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
  @JsonProperty(value ="name")
  private String name;

  private ApiObjectFieldDTO(List<String> requiredValues, String type, List<ApiTypeArrayDTO> properties, List<Object> defaultValues, String name) {
    this.requiredValues = requiredValues;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.name = name;

  }

  private ApiObjectFieldDTO(ApiObjectFieldDTOBuilder builder) {
    this.requiredValues = builder.requiredValues;
    this.type = builder.type;
    this.properties = builder.properties;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;

  }

  public static ApiObjectFieldDTO.ApiObjectFieldDTOBuilder builder() {
    return new ApiObjectFieldDTO.ApiObjectFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiObjectFieldDTOBuilder {

    private List<String> requiredValues = new ArrayList<String>();
    private String type;
    private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
    private List<Object> defaultValues = new ArrayList<Object>();
    private String name;
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder requiredValues(List<String> requiredValues) {
      if (!requiredValues.isEmpty()) {
        this.requiredValues.addAll(requiredValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder requiredValue(String requiredValue) {
      if (requiredValue != null) {
        this.requiredValues.add(requiredValue);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder properties(List<ApiTypeArrayDTO> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder propertie(ApiTypeArrayDTO propertie) {
      if (propertie != null) {
        this.properties.add(propertie);
      }
      return this;
    }
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder defaultValue(Object defaultValue) {
      if (defaultValue != null) {
        this.defaultValues.add(defaultValue);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiObjectFieldDTO build() {
      ApiObjectFieldDTO apiObjectFieldDTO = new ApiObjectFieldDTO(this);
      return apiObjectFieldDTO;
    }
  }

  @Schema(name = "requiredValues", required = false)
  public List<String> getRequiredValues() {
    return requiredValues;
  }
  public void setRequiredValues(List<String> requiredValues) {
    this.requiredValues = requiredValues;
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

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiObjectFieldDTO apiObjectFieldDTO = (ApiObjectFieldDTO) o;
    return Objects.equals(this.requiredValues, apiObjectFieldDTO.requiredValues) && Objects.equals(this.type, apiObjectFieldDTO.type) && Objects.equals(this.properties, apiObjectFieldDTO.properties) && Objects.equals(this.defaultValues, apiObjectFieldDTO.defaultValues) && Objects.equals(this.name, apiObjectFieldDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(requiredValues, type, properties, defaultValues, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiObjectFieldDTO{");
    sb.append(" requiredValues:").append(requiredValues).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" properties:").append(properties).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }


}
