package com.sngular.multifileplugin.testadditionalpropertiesWithSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class SubtestDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;


  @Builder
  @Jacksonized
  private SubtestDTO(@NonNull String name, @NonNull Integer code) {
    this.name = name;
    this.code = code;

  }

}