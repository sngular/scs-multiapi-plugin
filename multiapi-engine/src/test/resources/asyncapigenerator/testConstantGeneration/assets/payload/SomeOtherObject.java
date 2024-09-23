package com.sngular.scsplugin.constantgeneration.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class SomeOtherObject {

  @JsonProperty(value ="property2")
  private Integer property2 = 6;

  @JsonProperty(value ="property1")
  private Integer property1 = 3;


  @Builder
  @Jacksonized
  private SomeOtherObject(Integer property2, Integer property1) {
    this.property2 = property2;
    this.property1 = property1;

  }

}
