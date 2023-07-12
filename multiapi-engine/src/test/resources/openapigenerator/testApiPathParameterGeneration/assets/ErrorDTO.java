package com.sngular.multifileplugin.pathparameter.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.pathparameter.model.exception.ModelClassException;
import com.sngular.multifileplugin.pathparameter.model.customvalidator.NotNull;

@JsonDeserialize(builder = ErrorDTO.ErrorDTOBuilder.class)
public class ErrorDTO {

  @JsonProperty(value ="code")
  @NotNull
  private final Integer code;
  @JsonProperty(value ="message")
  @NotNull
  private final String message;

  private ErrorDTO(Integer code, String message) {
    this.code = code;
    this.message = message;

    validateRequiredAttributes();
  }

  private ErrorDTO(ErrorDTOBuilder builder) {
    this.code = builder.code;
    this.message = builder.message;

    validateRequiredAttributes();
  }

  public static ErrorDTO.ErrorDTOBuilder builder() {
    return new ErrorDTO.ErrorDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ErrorDTOBuilder {

    private Integer code;
    private String message;

    public ErrorDTO.ErrorDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public ErrorDTO.ErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ErrorDTO build() {
      ErrorDTO errorDTO = new ErrorDTO(this);
      return errorDTO;
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
    ErrorDTO errorDTO = (ErrorDTO) o;
    return Objects.equals(this.code, errorDTO.code) && Objects.equals(this.message, errorDTO.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(code, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ErrorDTO{");
    sb.append(" code:").append(toIndentedString(code)).append(",");
    sb.append(" message:").append(toIndentedString(message)).append(",");
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
      throw new ModelClassException("ErrorDTO");
    }
  }

}
