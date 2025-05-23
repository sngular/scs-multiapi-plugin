package com.sngular.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testapi.model.exception.ModelClassException;
import com.sngular.multifileplugin.testapi.model.customvalidator.NotNull;

@JsonDeserialize(builder = ApiErrorDTO.ApiErrorDTOBuilder.class)
public class ApiErrorDTO {

  @JsonProperty(value ="code")
  @NotNull
  private final Integer code;
  @JsonProperty(value ="message")
  @NotNull
  private final String message;

  private ApiErrorDTO(ApiErrorDTOBuilder builder) {
    this.code = builder.code;
    this.message = builder.message;

    validateRequiredAttributes();
  }

  public static ApiErrorDTO.ApiErrorDTOBuilder builder() {
    return new ApiErrorDTO.ApiErrorDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiErrorDTOBuilder {

    private Integer code;
    private String message;

    public ApiErrorDTO.ApiErrorDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ApiErrorDTO build() {
      ApiErrorDTO apiErrorDTO = new ApiErrorDTO(this);
      return apiErrorDTO;
    }
  }

  @Schema(name = "code", required = true)
  public Integer getCode() {
    return code;
  }

  @Schema(name = "message", required = true)
  public String getMessage() {
    return message;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiErrorDTO apiErrorDTO = (ApiErrorDTO) o;
    return Objects.equals(this.code, apiErrorDTO.code) && Objects.equals(this.message, apiErrorDTO.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(code, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiErrorDTO{");
    sb.append(" code:").append(code).append(",");
    sb.append(" message:").append(message);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.code)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.message)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiErrorDTO");
    }
  }

}
