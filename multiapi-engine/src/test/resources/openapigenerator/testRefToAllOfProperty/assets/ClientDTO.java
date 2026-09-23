package com.sngular.multifileplugin.testreftoallofproperty.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ClientDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="document_number")
  @NonNull
  private String document_number;

  @JsonProperty(value ="client_id")
  @NonNull
  private Long client_id;


  @Builder
  @Jacksonized
  private ClientDTO(@NonNull String name, @NonNull String document_number, @NonNull Long client_id) {
    this.name = name;
    this.document_number = document_number;
    this.client_id = client_id;

  }

}
