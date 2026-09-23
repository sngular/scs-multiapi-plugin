package com.sngular.multifileplugin.testreftoallofproperty.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ClientSearchResultDTO {

  @JsonProperty(value ="tags")
  @Singular("tag")
  private List<String> tags;

  @JsonProperty(value ="client_found")
  private ClientDTO client_found;


  @Builder
  @Jacksonized
  private ClientSearchResultDTO(List<String> tags, ClientDTO client_found) {
    this.tags = tags;
    this.client_found = client_found;

  }

}
