package com.sngular.multifileplugin.testReferenceFile.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestDTO {

  @JsonProperty(value ="something")
  private String something;


  @Builder
  @Jacksonized
  private TestDTO(String something) {
    this.something = something;

  }

}
