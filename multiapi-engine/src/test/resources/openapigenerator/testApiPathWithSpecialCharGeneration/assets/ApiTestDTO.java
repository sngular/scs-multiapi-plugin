package com.sngular.multifileplugin.pathwithspecialchar.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.pathwithspecialchar.model.exception.ModelClassException;
import com.sngular.multifileplugin.pathwithspecialchar.model.customvalidator.NotNull;

@JsonDeserialize(builder = ApiTestDTO.ApiTestDTOBuilder.class)
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="id")
  @NotNull
  private final Integer id;

  private ApiTestDTO(String name, Integer id) {
    this.name = name;
    this.id = id;

    validateRequiredAttributes();
  }

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

    validateRequiredAttributes();
  }

  public static ApiTestDTO.ApiTestDTOBuilder builder() {
    return new ApiTestDTO.ApiTestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestDTOBuilder {

    private String name;
    private Integer id;

    public ApiTestDTO.ApiTestDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO = new ApiTestDTO(this);
      return apiTestDTO;
    }
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Schema(name = "id", required = true)
  public Integer getId() {
    return id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestDTO apiTestDTO = (ApiTestDTO) o;
    return Objects.equals(this.name, apiTestDTO.name) && Objects.equals(this.id, apiTestDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.id)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTestDTO");
    }
  }

}
