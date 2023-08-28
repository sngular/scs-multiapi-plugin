package com.sngular.scsplugin.infiniteLoop.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ConfigDTO.ConfigDTOBuilder.class)
public class ConfigDTO {

  @JsonProperty(value ="name")
  private String name;

  private ConfigDTO(String name) {
    this.name = name;

  }

  private ConfigDTO(ConfigDTOBuilder builder) {
    this.name = builder.name;

  }

  public static ConfigDTO.ConfigDTOBuilder builder() {
    return new ConfigDTO.ConfigDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ConfigDTOBuilder {

    private String name;

    public ConfigDTO.ConfigDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ConfigDTO build() {
      ConfigDTO configDTO = new ConfigDTO(this);
      return configDTO;
    }
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ConfigDTO configDTO = (ConfigDTO) o;
    return Objects.equals(this.name, configDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ConfigDTO{");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }




}
