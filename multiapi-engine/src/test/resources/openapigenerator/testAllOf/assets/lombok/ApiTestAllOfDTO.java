package com.sngular.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ApiTestAllOfDTO {

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

  @JsonProperty(value ="testName")
  private String testName;


  @Builder
  @Jacksonized
  private ApiTestAllOfDTO(List<String> testers, String testName) {
    this.testers = testers;
    this.testName = testName;

  }

}
