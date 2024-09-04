package com.sngular.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ApiTestDTO {

  @JsonProperty(value ="testers")
  @NonNull
  private List<String> testers;

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
