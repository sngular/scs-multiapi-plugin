package com.sngular.multifileplugin.testapiclient.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;


  @Builder
  @Jacksonized
  private ApiTestDTO(@NonNull String name, @NonNull Integer id) {
    this.name = name;
    this.id = id;

  }

}
