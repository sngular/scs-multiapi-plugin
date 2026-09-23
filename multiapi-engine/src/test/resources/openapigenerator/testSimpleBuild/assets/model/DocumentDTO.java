package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.web.multipart.MultipartFile;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class DocumentDTO {

  @JsonProperty(value ="description")
  private String description;

  @JsonProperty(value ="attachments")
  @Singular(value = "attachment", ignoreNullCollections = true)
  private List<MultipartFile> attachments;

  @JsonProperty(value ="document")
  private MultipartFile document;


  @Builder
  @Jacksonized
  private DocumentDTO(String description, List<MultipartFile> attachments, MultipartFile document) {
    this.description = description;
    this.attachments = attachments;
    this.document = document;

  }

}
