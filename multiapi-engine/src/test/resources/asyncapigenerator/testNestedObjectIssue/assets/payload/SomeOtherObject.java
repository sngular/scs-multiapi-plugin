package com.sngular.scsplugin.nestedobject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class SomeOtherObject {

  @JsonProperty(value ="property2")
  private Integer property2;

  @JsonProperty(value ="property1")
  private String property1;


  @Builder
  @Jacksonized
  private SomeOtherObject(Integer property2, String property1) {
    this.property2 = property2;
    this.property1 = property1;

  }

}
