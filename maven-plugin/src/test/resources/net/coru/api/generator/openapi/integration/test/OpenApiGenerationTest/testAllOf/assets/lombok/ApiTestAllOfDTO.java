package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testAllOf.assets.lombok;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;


@Data
public class ApiTestAllOfDTO {

  @JsonProperty(value ="testName")
  private String testName;

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();


  @Builder
  private ApiTestAllOfDTO(String testName, List<String> testers){
    this.testName = testName;
    this.testers = testers;

  }

}
