package com.sngular.multifileplugin.lombok.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class ApiTestDTO {

  @JsonProperty(value ="testers")
  @NonNull
  private List<String> testers = new ArrayList<String>();

  @JsonProperty(value ="testName")
  @NonNull
  private String testName;


  @Builder
  @Jacksonized
  private ApiTestDTO(@NonNull List<String> testers, @NonNull String testName) {
    this.testers = testers;
    this.testName = testName;

  }

}
