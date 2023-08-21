package com.sngular.multifileplugin.testissuefaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class FakerFieldDTO {

  @JsonProperty(value ="value")
  private Object value;

  @JsonProperty(value ="type")
  @NonNull
  private String type;

  @JsonProperty(value ="name")
  @NonNull
  private String name;


  @Builder
  @Jacksonized
  private FakerFieldDTO(Object value, @NonNull String type, @NonNull String name) {
    this.value = value;
    this.type = type;
    this.name = name;

  }

}
