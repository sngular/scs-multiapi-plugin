package com.sngular.multifileplugin.lombok.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import javax.validation.constraints.Size;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class ApiErrorDTO {

  @Min(10)
  @Max(200)
  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

  @Size(min =50, max =200)
  @JsonProperty(value ="message")
  @NonNull
  private String message;


  @Builder
  @Jacksonized
  private ApiErrorDTO(@NonNull Integer code, @NonNull String message) {
    this.code = code;
    this.message = message;

  }

}
