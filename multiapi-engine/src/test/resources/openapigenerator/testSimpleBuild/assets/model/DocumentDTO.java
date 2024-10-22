package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.web.multipart.MultipartFile;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class DocumentDTO {

  @JsonProperty(value ="description")
  private String description;

  @JsonProperty(value ="document")
  private MultipartFile document;


  @Builder
  @Jacksonized
  private DocumentDTO(String description, MultipartFile document) {
    this.description = description;
    this.document = document;

  }

}
