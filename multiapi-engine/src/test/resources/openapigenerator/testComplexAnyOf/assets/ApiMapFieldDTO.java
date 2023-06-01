package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiMapFieldDTO.ApiMapFieldDTOBuilder.class)
public class ApiMapFieldDTO {

  @JsonProperty(value ="keyType")
  private String keyType;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="mapSize")
  private Integer mapSize;
  @JsonProperty(value ="mapTypes")
  private List<ApiTypeArrayDTO> mapTypes = new ArrayList<ApiTypeArrayDTO>();

  private ApiMapFieldDTO(String keyType, String type, List<Object> defaultValues, String name, Integer mapSize, List<ApiTypeArrayDTO> mapTypes) {
    this.keyType = keyType;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.mapSize = mapSize;
    this.mapTypes = mapTypes;

  }

  private ApiMapFieldDTO(ApiMapFieldDTOBuilder builder) {
    this.keyType = builder.keyType;
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.mapSize = builder.mapSize;
    this.mapTypes = builder.mapTypes;

  }

  public static ApiMapFieldDTO.ApiMapFieldDTOBuilder builder() {
    return new ApiMapFieldDTO.ApiMapFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiMapFieldDTOBuilder {

    private String keyType;
    private String type;
    private List<Object> defaultValues = new ArrayList<Object>();
    private String name;
    private Integer mapSize;
    private List<ApiTypeArrayDTO> mapTypes = new ArrayList<ApiTypeArrayDTO>();

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder keyType(String keyType) {
      this.keyType = keyType;
      return this;
    }

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiMapFieldDTO.ApiMapFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder defaultValues(Object defaultValues) {
      if (defaultValues != null) {
        this.defaultValues.add(defaultValues);
      }
      return this;
    }

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder mapSize(Integer mapSize) {
      this.mapSize = mapSize;
      return this;
    }
    public ApiMapFieldDTO.ApiMapFieldDTOBuilder mapTypes(List<ApiTypeArrayDTO> mapTypes) {
      if (!mapTypes.isEmpty()) {
        this.mapTypes.addAll(mapTypes);
      }
      return this;
    }

    public ApiMapFieldDTO.ApiMapFieldDTOBuilder mapTypes(ApiTypeArrayDTO mapTypes) {
      if (mapTypes != null) {
        this.mapTypes.add(mapTypes);
      }
      return this;
    }

    public ApiMapFieldDTO build() {
      ApiMapFieldDTO apiMapFieldDTO = new ApiMapFieldDTO(this);
      return apiMapFieldDTO;
    }
  }


  @Schema(name = "keyType", required = false)
  public String getKeyType() {
    return keyType;
  }
  public void setKeyType(String keyType) {
    this.keyType = keyType;
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiMapFieldDTO apiMapFieldDTO = (ApiMapFieldDTO) o;
    return Objects.equals(this.keyType, apiMapFieldDTO.keyType) && Objects.equals(this.type, apiMapFieldDTO.type) && Objects.equals(this.defaultValues, apiMapFieldDTO.defaultValues) && Objects.equals(this.name, apiMapFieldDTO.name) && Objects.equals(this.mapSize, apiMapFieldDTO.mapSize) && Objects.equals(this.mapTypes, apiMapFieldDTO.mapTypes);
  }

  @Override
  public int hashCode() {
    return Objects.hash(keyType, type, defaultValues, name, mapSize, mapTypes);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiMapFieldDTO {\n");
    sb.append(" keyType: ").append(toIndentedString(keyType)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" mapSize: ").append(toIndentedString(mapSize)).append("\n");
    sb.append(" mapTypes: ").append(toIndentedString(mapTypes)).append("\n");
    sb.append("}");
    return sb.toString();
  }


  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }


}
