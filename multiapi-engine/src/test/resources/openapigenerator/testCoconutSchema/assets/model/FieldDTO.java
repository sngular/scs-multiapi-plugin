package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class FieldDTO {

  @JsonProperty(value ="field")
  private Object field;


  @Builder
  @Jacksonized
  private FieldDTO(Object field) {
    this.field = field;

  }

}
