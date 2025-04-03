package com.sngular.scsplugin.noschemas.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class Thing {

  @JsonProperty(value ="id")
  private Integer id;


  @Builder
  @Jacksonized
  private Thing(Integer id) {
    this.id = id;

  }

}
