package com.sngular.multifileplugin.testreferencefilenocomponents.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class InlineResponse200TestFileDTO {

  @JsonProperty(value ="something")
  private String something;


  @Builder
  @Jacksonized
  private InlineResponse200TestFileDTO(String something) {
    this.something = something;

  }

}
