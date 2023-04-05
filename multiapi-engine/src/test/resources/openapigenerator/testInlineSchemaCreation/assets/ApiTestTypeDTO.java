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
  * Get id
  * @return id
  */
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
    sb.append("class ApiTestTypeDTO {\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
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
