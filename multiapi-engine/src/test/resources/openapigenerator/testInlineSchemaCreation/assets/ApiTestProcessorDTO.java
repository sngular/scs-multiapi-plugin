package com.sngular.multifileplugin.inlineschemacreation.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiTestProcessorDTO.ApiTestProcessorDTOBuilder.class)
public class ApiTestProcessorDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="id")
  private String id;

  private ApiTestProcessorDTO(String name, String id) {
    this.name = name;
    this.id = id;

  }

  private ApiTestProcessorDTO(ApiTestProcessorDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

  }

  public static ApiTestProcessorDTO.ApiTestProcessorDTOBuilder builder() {
    return new ApiTestProcessorDTO.ApiTestProcessorDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestProcessorDTOBuilder {

    private String name;
    private String id;

    public ApiTestProcessorDTO.ApiTestProcessorDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestProcessorDTO.ApiTestProcessorDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ApiTestProcessorDTO build() {
      ApiTestProcessorDTO apiTestProcessorDTO = new ApiTestProcessorDTO(this);
      return apiTestProcessorDTO;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestProcessorDTO apiTestProcessorDTO = (ApiTestProcessorDTO) o;
    return Objects.equals(this.name, apiTestProcessorDTO.name) && Objects.equals(this.id, apiTestProcessorDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestProcessorDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
