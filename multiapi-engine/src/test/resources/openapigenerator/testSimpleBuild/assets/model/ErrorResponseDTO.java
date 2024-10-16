package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ErrorResponseDTO {

  @JsonProperty(value ="status")
  private String status;

  @JsonProperty(value ="message")
  private String message;


  @Builder
  @Jacksonized
  private ErrorResponseDTO(String status, String message) {
    this.status = status;
    this.message = message;

  }

}
