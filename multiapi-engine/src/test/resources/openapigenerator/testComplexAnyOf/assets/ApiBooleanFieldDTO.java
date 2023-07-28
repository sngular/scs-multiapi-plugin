package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder.class)
public class ApiBooleanFieldDTO {

  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="defaultValue")
  private Boolean defaultValue;

  private ApiBooleanFieldDTO(String type, String name, Boolean defaultValue) {
    this.type = type;
    this.name = name;
    this.defaultValue = defaultValue;

  }

  private ApiBooleanFieldDTO(ApiBooleanFieldDTOBuilder builder) {
    this.type = builder.type;
    this.name = builder.name;
    this.defaultValue = builder.defaultValue;

  }

  public static ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder builder() {
    return new ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiBooleanFieldDTOBuilder {

    private String type;
    private String name;
    private Boolean defaultValue;

    public ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiBooleanFieldDTO.ApiBooleanFieldDTOBuilder defaultValue(Boolean defaultValue) {
      this.defaultValue = defaultValue;
      return this;
    }

    public ApiBooleanFieldDTO build() {
      ApiBooleanFieldDTO apiBooleanFieldDTO = new ApiBooleanFieldDTO(this);
      return apiBooleanFieldDTO;
    }
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "defaultValue", required = false)
  public Boolean getDefaultValue() {
    return defaultValue;
  }
  public void setDefaultValue(Boolean defaultValue) {
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
    ApiBooleanFieldDTO apiBooleanFieldDTO = (ApiBooleanFieldDTO) o;
    return Objects.equals(this.type, apiBooleanFieldDTO.type) && Objects.equals(this.name, apiBooleanFieldDTO.name) && Objects.equals(this.defaultValue, apiBooleanFieldDTO.defaultValue);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, name, defaultValue);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiBooleanFieldDTO{");
    sb.append(" type:").append(type).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" defaultValue:").append(defaultValue);
    sb.append("}");
    return sb.toString();
  }


}
