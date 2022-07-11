package net.coru.multifileplugin.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

public class ApiTestDTO {

  @JsonProperty(value ="testName")
  private final String testName;
  @JsonProperty(value ="testers")
  private final List<String> testers;

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.testName = builder.testName;
    this.testers = builder.testers;
  }

  public static class ApiTestDTOBuilder {

    private String testName;
    private List<String> testers = new ArrayList<String>();

    public ApiTestDTO.ApiTestDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder tester(String tester) {
      if (tester != null) {
        this.testers.add(tester);
      }
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO =  new ApiTestDTO(this);
      return apiTestDTO;
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
    ApiTestDTO apiTestDTO = (ApiTestDTO) o;
    return Objects.equals(this.testName,apiTestDTO.testName) && Objects.equals(this.testers,apiTestDTO.testers) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(testName,testers);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestDTO {\n");
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

}
