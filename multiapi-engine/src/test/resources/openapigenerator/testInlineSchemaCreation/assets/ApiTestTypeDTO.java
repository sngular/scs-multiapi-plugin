package com.sngular.multifileplugin.inlineschemacreation.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiTestTypeDTO.ApiTestTypeDTOBuilder.class)
public class ApiTestTypeDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="id")
  private String id;

  private ApiTestTypeDTO(String name, String id) {
    this.name = name;
    this.id = id;

  }

  private ApiTestTypeDTO(ApiTestTypeDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

  }

  public static ApiTestTypeDTO.ApiTestTypeDTOBuilder builder() {
    return new ApiTestTypeDTO.ApiTestTypeDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestTypeDTOBuilder {

    private String name;
    private String id;

    public ApiTestTypeDTO.ApiTestTypeDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestTypeDTO.ApiTestTypeDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ApiTestTypeDTO build() {
      ApiTestTypeDTO apiTestTypeDTO = new ApiTestTypeDTO(this);
      return apiTestTypeDTO;
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
    ApiTestTypeDTO apiTestTypeDTO = (ApiTestTypeDTO) o;
    return Objects.equals(this.name, apiTestTypeDTO.name) && Objects.equals(this.id, apiTestTypeDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestTypeDTO{");
    sb.append(" name:").append(toIndentedString(name)).append(",");
    sb.append(" id:").append(toIndentedString(id)).append(",");
    sb.append("}");
    return sb.toString();
  }


}
