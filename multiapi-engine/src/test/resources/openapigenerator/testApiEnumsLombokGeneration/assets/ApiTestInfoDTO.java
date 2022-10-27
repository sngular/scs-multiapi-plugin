package net.coru.multifileplugin.enumlombokgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class ApiTestInfoDTO {

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

  @JsonProperty(value ="testName")
  @NonNull
  private String testName;


  @Builder
  private ApiTestInfoDTO(List<String> testers, @NonNull String testName) {
    this.testers = testers;
    this.testName = testName;

  }

}
