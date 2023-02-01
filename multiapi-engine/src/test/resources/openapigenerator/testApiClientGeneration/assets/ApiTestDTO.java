package com.sngular.multifileplugin.testapiclient.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;


  @Builder
  @JsonPOJOBuilder
  private ApiTestDTO(@NonNull String name, @NonNull Integer id) {
    this.name = name;
    this.id = id;

  }

}
