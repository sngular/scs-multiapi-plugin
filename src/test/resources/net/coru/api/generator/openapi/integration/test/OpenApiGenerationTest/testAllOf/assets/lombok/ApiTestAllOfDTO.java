package net.coru.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiTestAllOfDTO {

  @JsonProperty(value ="testName")
  private String testName;

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

}
