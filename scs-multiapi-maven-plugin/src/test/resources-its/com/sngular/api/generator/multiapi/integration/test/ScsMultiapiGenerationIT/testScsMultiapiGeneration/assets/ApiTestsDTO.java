package com.sngular.generator.multiapi.model.event.consumer;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class ApiTestsDTO {

  @JsonProperty(value ="apiTestDTO")
  private ApiTestDTO apiTestDTO;

  private ApiTestsDTO(ApiTestDTO apiTestDTO) {
    this.apiTestDTO = apiTestDTO;

  }

  private ApiTestsDTO(ApiTestsDTOBuilder builder) {
    this.apiTestDTO = builder.apiTestDTO;

  }

  public static class ApiTestsDTOBuilder {

    private ApiTestDTO apiTestDTO;

    public ApiTestsDTO.ApiTestsDTOBuilder apiTestDTO(ApiTestDTO apiTestDTO) {
      this.apiTestDTO = apiTestDTO;
      return this;
    }

    public ApiTestsDTO build() {
      ApiTestsDTO apiTestsDTO = new ApiTestsDTO(this);
      return apiTestsDTO;
    }
  }

  /**
  * Get apiTestDTO
  * @return apiTestDTO
  */
  @Schema(name = "apiTestDTO", required = false)
  public ApiTestDTO getApiTestDTO() {
    return apiTestDTO;
  }
  public void setApiTestDTO(ApiTestDTO apiTestDTO) {
    this.apiTestDTO = apiTestDTO;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestsDTO apiTestsDTO = (ApiTestsDTO) o;
    return Objects.equals(this.apiTestDTO, apiTestsDTO.apiTestDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(apiTestDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestsDTO {\n");
    sb.append(" apiTestDTO: ").append(toIndentedString(apiTestDTO)).append("\n");
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
