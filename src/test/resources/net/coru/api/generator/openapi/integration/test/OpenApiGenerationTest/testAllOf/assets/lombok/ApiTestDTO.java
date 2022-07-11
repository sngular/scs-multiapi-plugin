package net.coru.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
@Builder
public class ApiTestDTO {

  @JsonProperty(value ="testName")
  @NonNull
  private String testName;

  @JsonProperty(value ="testers")
  @NonNull
  private List<String> testers = new ArrayList<String>();

}
