package com.sngular.multifileplugin.parameterwithcontent.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ApiInlineParameterTestIDDTO.ApiInlineParameterTestIDDTOBuilder.class)
public class ApiInlineParameterTestIDDTO {

  @JsonProperty(value ="email")
  private String email;

  private ApiInlineParameterTestIDDTO(ApiInlineParameterTestIDDTOBuilder builder) {
    this.email = builder.email;

  }

  public static ApiInlineParameterTestIDDTO.ApiInlineParameterTestIDDTOBuilder builder() {
    return new ApiInlineParameterTestIDDTO.ApiInlineParameterTestIDDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiInlineParameterTestIDDTOBuilder {

    private String email;

    public ApiInlineParameterTestIDDTO.ApiInlineParameterTestIDDTOBuilder email(String email) {
      this.email = email;
      return this;
    }

    public ApiInlineParameterTestIDDTO build() {
      ApiInlineParameterTestIDDTO apiInlineParameterTestIDDTO = new ApiInlineParameterTestIDDTO(this);
      return apiInlineParameterTestIDDTO;
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
    ApiInlineParameterTestIDDTO apiInlineParameterTestIDDTO = (ApiInlineParameterTestIDDTO) o;
    return Objects.equals(this.email, apiInlineParameterTestIDDTO.email);
  }

  @Override
  public int hashCode() {
    return Objects.hash(email);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiInlineParameterTestIDDTO{");
    sb.append(" email:").append(email);
    sb.append("}");
    return sb.toString();
  }


}
