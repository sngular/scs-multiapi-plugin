package net.coru.multifileplugin.lombok.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;


@Data
public class ApiTestInfoDTO {

  @JsonProperty(value ="testName")
  @NonNull
  private String testName;

  @JsonProperty(value ="testers")
  @NonNull
  private List<String> testers = new ArrayList<String>();


  @Builder
  private ApiTestInfoDTO(@NonNull String testName, @NonNull List<String> testers) {
    this.testName = testName;
    this.testers = testers;

  }

}
