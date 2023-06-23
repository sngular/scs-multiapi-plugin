package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiDateFieldDTO.ApiDateFieldDTOBuilder.class)
public class ApiDateFieldDTO {

  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private Object defaultValues;
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="format")
  private String format;

  private ApiDateFieldDTO(String type, Object defaultValues, String name, String format) {
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.format = format;

  }

  private ApiDateFieldDTO(ApiDateFieldDTOBuilder builder) {
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.format = builder.format;

  }

  public static ApiDateFieldDTO.ApiDateFieldDTOBuilder builder() {
    return new ApiDateFieldDTO.ApiDateFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiDateFieldDTOBuilder {

    private String type;
    private Object defaultValues;
    private String name;
    private String format;

    public ApiDateFieldDTO.ApiDateFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiDateFieldDTO.ApiDateFieldDTOBuilder defaultValues(Object defaultValues) {
      this.defaultValues = defaultValues;
      return this;
    }

    public ApiDateFieldDTO.ApiDateFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiDateFieldDTO.ApiDateFieldDTOBuilder format(String format) {
      this.format = format;
      return this;
    }

    public ApiDateFieldDTO build() {
      ApiDateFieldDTO apiDateFieldDTO = new ApiDateFieldDTO(this);
      return apiDateFieldDTO;
    }
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "defaultValues", required = false)
  public Object getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(Object defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "format", required = false)
  public String getFormat() {
    return format;
  }
  public void setFormat(String format) {
    this.format = format;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiDateFieldDTO apiDateFieldDTO = (ApiDateFieldDTO) o;
    return Objects.equals(this.type, apiDateFieldDTO.type) && Objects.equals(this.defaultValues, apiDateFieldDTO.defaultValues) && Objects.equals(this.name, apiDateFieldDTO.name) && Objects.equals(this.format, apiDateFieldDTO.format);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, defaultValues, name, format);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiDateFieldDTO {\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" format: ").append(toIndentedString(format)).append("\n");
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
