package com.sngular.multifileplugin.testreftoallofproperty.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ClientDataDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="document_number")
  private String document_number;


  @Builder
  @Jacksonized
  private ClientDataDTO(@NonNull String name, String document_number) {
    this.name = name;
    this.document_number = document_number;

  }

}
