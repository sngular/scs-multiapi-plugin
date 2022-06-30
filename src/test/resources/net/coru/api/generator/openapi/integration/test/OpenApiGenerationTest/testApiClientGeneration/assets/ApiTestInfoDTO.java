package net.coru.multifileplugin.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ApiTestInfoDTO {

  @JsonProperty(value ="testName")
  @NotNull
  private String testName;
  @JsonProperty(value ="testers")
  @NotNull
  private List<String> testers = new ArrayList<String>();

}
