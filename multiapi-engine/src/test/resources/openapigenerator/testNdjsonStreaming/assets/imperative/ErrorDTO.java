package com.sngular.multifileplugin.ndjsonstreaming.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ErrorDTO.ErrorDTOBuilder.class)
public class ErrorDTO {

  @JsonProperty(value ="message")
  private String message;

  private ErrorDTO(ErrorDTOBuilder builder) {
    this.message = builder.message;

  }

  public static ErrorDTO.ErrorDTOBuilder builder() {
    return new ErrorDTO.ErrorDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ErrorDTOBuilder {

    private String message;

    public ErrorDTO.ErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ErrorDTO build() {
      ErrorDTO errorDTO = new ErrorDTO(this);
      return errorDTO;
    }
  }

  @Schema(name = "message", required = false)
  public String getMessage() {
    return message;
  }
  public void setMessage(String message) {
    this.message = message;
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
    return Objects.equals(this.message, errorDTO.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ErrorDTO{");
    sb.append(" message:").append(message);
    sb.append("}");
    return sb.toString();
  }


}
