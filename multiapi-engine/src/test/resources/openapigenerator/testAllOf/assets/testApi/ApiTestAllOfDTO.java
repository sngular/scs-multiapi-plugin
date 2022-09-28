package net.coru.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

public class ApiTestAllOfDTO {

  @JsonProperty(value ="testName")
  private String testName;
  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

  private ApiTestAllOfDTO(String testName, List<String> testers) {
    this.testName = testName;
    this.testers = testers;

  }

  private ApiTestAllOfDTO(ApiTestAllOfDTOBuilder builder) {
    this.testName = builder.testName;
    this.testers = builder.testers;

  }

  public static class ApiTestAllOfDTOBuilder {

    private String testName;
    private List<String> testers = new ArrayList<String>();

    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }
    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder tester(String tester) {
      if (tester != null) {
        this.testers.add(tester);
      }
      return this;
    }

    public ApiTestAllOfDTO build() {
      ApiTestAllOfDTO apiTestAllOfDTO = new ApiTestAllOfDTO(this);
      return apiTestAllOfDTO;
    }
  }

  /**
  * Get testName
  * @return testName
  */
  @Schema(name = "testName", required = false)
  public String getTestName() {
    return testName;
  }
  public void setTestName(String testName) {
    this.testName = testName;
  }

  /**
  * Get testers
  * @return testers
  */
  @Schema(name = "testers", required = false)
  public List<String> getTesters() {
    return testers;
  }
  public void setTesters(List<String> testers) {
    this.testers = testers;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestAllOfDTO apiTestAllOfDTO = (ApiTestAllOfDTO) o;
    return Objects.equals(this.testName, apiTestAllOfDTO.testName) && Objects.equals(this.testers, apiTestAllOfDTO.testers);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testName, testers);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestAllOfDTO {\n");
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
