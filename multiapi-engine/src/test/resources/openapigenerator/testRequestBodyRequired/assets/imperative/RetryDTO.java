package com.sngular.multifileplugin.requestbodyrequired.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = RetryDTO.RetryDTOBuilder.class)
public class RetryDTO {

  @JsonProperty(value ="reason")
  private String reason;

  private RetryDTO(RetryDTOBuilder builder) {
    this.reason = builder.reason;

  }

  public static RetryDTO.RetryDTOBuilder builder() {
    return new RetryDTO.RetryDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class RetryDTOBuilder {

    private String reason;

    public RetryDTO.RetryDTOBuilder reason(String reason) {
      this.reason = reason;
      return this;
    }

    public RetryDTO build() {
      RetryDTO retryDTO = new RetryDTO(this);
      return retryDTO;
    }
  }

  @Schema(name = "reason", required = false)
  public String getReason() {
    return reason;
  }
  public void setReason(String reason) {
    this.reason = reason;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RetryDTO retryDTO = (RetryDTO) o;
    return Objects.equals(this.reason, retryDTO.reason);
  }

  @Override
  public int hashCode() {
    return Objects.hash(reason);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("RetryDTO{");
    sb.append(" reason:").append(reason);
    sb.append("}");
    return sb.toString();
  }


}
