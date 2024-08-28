package com.sngular.multifileplugin.testissuefaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.Singular;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Value
public class FieldDTO {

  @JsonProperty(value ="type")
  @NonNull
  private String type;

  @JsonProperty(value ="name")
  @NonNull
  private String name;


  @Builder
  @Jacksonized
  private FieldDTO(@NonNull String type, @NonNull String name) {
    this.type = type;
    this.name = name;

  }

}
