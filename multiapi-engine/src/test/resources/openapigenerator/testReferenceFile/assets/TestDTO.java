package com.sngular.multifileplugin.testReferenceFile.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestDTO {

  @JsonProperty(value ="something")
  private String something;


  @Builder
  @Jacksonized
  private TestDTO(String something) {
    this.something = something;

  }

}
