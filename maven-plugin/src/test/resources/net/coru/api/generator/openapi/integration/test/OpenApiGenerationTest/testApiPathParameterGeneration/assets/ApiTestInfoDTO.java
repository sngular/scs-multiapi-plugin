package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiPathParameterGeneration.assets;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiParametersWithContentGeneration.assets.exception.ModelClassException;


public class ApiTestInfoDTO {

  @JsonProperty(value ="testName")
  private final String testName;
  @JsonProperty(value ="testers")
  private final List<String> testers;

  private ApiTestInfoDTO(String testName, List<String> testers){
    this.testName = testName;
    this.testers = testers;

    validateRequiredAttributes();
  }

  private ApiTestInfoDTO(ApiTestInfoDTOBuilder builder) {
    this.testName = builder.testName;
    this.testers = builder.testers;

    validateRequiredAttributes();
  }

  public static class ApiTestInfoDTOBuilder {

    private String testName;
    private List<String> testers = new ArrayList<String>();

    public ApiTestInfoDTO.ApiTestInfoDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }
    public ApiTestInfoDTO.ApiTestInfoDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestInfoDTO.ApiTestInfoDTOBuilder tester(String tester) {
      if (tester != null) {
        this.testers.add(tester);
      }
      return this;
    }

    public ApiTestInfoDTO build() {
      ApiTestInfoDTO apiTestInfoDTO =  new ApiTestInfoDTO(this);
      return apiTestInfoDTO;
    }
  }

  /**
  * Get testName
  * @return testName
  */
  @Schema(name = "testName", required = true)
  public String getTestName() {
    return testName;
  }

  /**
  * Get testers
  * @return testers
  */
  @Schema(name = "testers", required = true)
  public List<String> getTesters() {
    return testers;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestInfoDTO apiTestInfoDTO = (ApiTestInfoDTO) o;
    return Objects.equals(this.testName,apiTestInfoDTO.testName) && Objects.equals(this.testers,apiTestInfoDTO.testers) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(testName,testers);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestInfoDTO {\n");
    sb.append(" testName: ").append(toIndentedString(testName)).append("\n");
    sb.append(" testers: ").append(toIndentedString(testers)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.testName)) {
      satisfiedCondition = false;
    }
    else if (!Objects.nonNull(this.testers)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTestInfoDTO");
    }
  }

}
