package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiTestInfoDTO {

  @JsonProperty(value ="testName")
  private String testName;

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

}
