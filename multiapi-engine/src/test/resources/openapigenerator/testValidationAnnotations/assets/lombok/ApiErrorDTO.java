package com.sngular.multifileplugin.lombok.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sngular.multifileplugin.testapi.model.customvalidator.Size;
import com.sngular.multifileplugin.testapi.model.customvalidator.Max;
import com.sngular.multifileplugin.testapi.model.customvalidator.Min;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class ApiErrorDTO {

  @JsonProperty(value ="code")
  @Min(10)
  @Max(200)
  @NonNull
  private Integer code;

  @JsonProperty(value ="message")
  @Size(min =50, max =200)
  @NonNull
  private String message;


  @Builder
  @Jacksonized
  private ApiErrorDTO(@NonNull Integer code, @NonNull String message) {
    this.code = code;
    this.message = message;

  }

}
