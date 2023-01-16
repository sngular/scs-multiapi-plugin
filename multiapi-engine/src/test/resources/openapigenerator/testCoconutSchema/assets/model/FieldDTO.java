package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
public class FieldDTO {

  @JsonProperty(value ="field")
  private Object field;


  @Builder
  private FieldDTO(Object field) {
    this.field = field;

  }

}
