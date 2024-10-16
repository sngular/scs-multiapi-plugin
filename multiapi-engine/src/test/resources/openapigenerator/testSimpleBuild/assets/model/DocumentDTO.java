package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class DocumentDTO {

  @JsonProperty(value ="description")
  private String description;

  @JsonProperty(value ="document")
  private String document;


  @Builder
  @Jacksonized
  private DocumentDTO(String description, String document) {
    this.description = description;
    this.document = document;

  }

}
