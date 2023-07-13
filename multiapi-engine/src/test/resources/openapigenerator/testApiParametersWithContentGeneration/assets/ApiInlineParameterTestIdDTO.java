package com.sngular.multifileplugin.parameterwithcontent.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiInlineParameterTestIdDTO.ApiInlineParameterTestIdDTOBuilder.class)
public class ApiInlineParameterTestIdDTO {

  @JsonProperty(value ="email")
  private String email;

  private ApiInlineParameterTestIdDTO(String email) {
    this.email = email;

  }

  private ApiInlineParameterTestIdDTO(ApiInlineParameterTestIdDTOBuilder builder) {
    this.email = builder.email;

  }

  public static ApiInlineParameterTestIdDTO.ApiInlineParameterTestIdDTOBuilder builder() {
    return new ApiInlineParameterTestIdDTO.ApiInlineParameterTestIdDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiInlineParameterTestIdDTOBuilder {

    private String email;

    public ApiInlineParameterTestIdDTO.ApiInlineParameterTestIdDTOBuilder email(String email) {
      this.email = email;
      return this;
    }

    public ApiInlineParameterTestIdDTO build() {
      ApiInlineParameterTestIdDTO apiInlineParameterTestIdDTO = new ApiInlineParameterTestIdDTO(this);
      return apiInlineParameterTestIdDTO;
    }
  }

  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiInlineParameterTestIdDTO apiInlineParameterTestIdDTO = (ApiInlineParameterTestIdDTO) o;
    return Objects.equals(this.email, apiInlineParameterTestIdDTO.email);
  }

  @Override
  public int hashCode() {
    return Objects.hash(email);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiInlineParameterTestIdDTO{");
    sb.append(" email:").append(email).append(",");
    sb.append("}");
    return sb.toString();
  }


}
