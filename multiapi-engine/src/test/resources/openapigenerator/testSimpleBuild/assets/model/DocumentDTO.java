package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
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
