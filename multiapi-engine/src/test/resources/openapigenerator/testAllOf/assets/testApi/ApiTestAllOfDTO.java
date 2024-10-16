package com.sngular.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiTestAllOfDTO.ApiTestAllOfDTOBuilder.class)
public class ApiTestAllOfDTO {

  @JsonProperty(value ="testers")
  private List<String> testers;
  @JsonProperty(value ="testName")
  private String testName;

  private ApiTestAllOfDTO(ApiTestAllOfDTOBuilder builder) {
    this.testers = builder.testers;
    this.testName = builder.testName;

  }

  public static ApiTestAllOfDTO.ApiTestAllOfDTOBuilder builder() {
    return new ApiTestAllOfDTO.ApiTestAllOfDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestAllOfDTOBuilder {

    private List<String> testers = new ArrayList<String>();
    private String testName;

    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder tester(String tester) {
      if (Objects.nonNull(tester)) {
        this.testers.add(tester);
      }
      return this;
    }

    public ApiTestAllOfDTO.ApiTestAllOfDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }

    public ApiTestAllOfDTO build() {
      ApiTestAllOfDTO apiTestAllOfDTO = new ApiTestAllOfDTO(this);
      return apiTestAllOfDTO;
    }
  }

  @Schema(name = "testers", required = false)
  public List<String> getTesters() {
    return testers;
  }
  public void setTesters(List<String> testers) {
    this.testers = testers;
  }

  @Schema(name = "testName", required = false)
  public String getTestName() {
    return testName;
  }
  public void setTestName(String testName) {
    this.testName = testName;
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
    return Objects.equals(this.testers, apiTestAllOfDTO.testers) && Objects.equals(this.testName, apiTestAllOfDTO.testName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testers, testName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestAllOfDTO{");
    sb.append(" testers:").append(testers).append(",");
    sb.append(" testName:").append(testName);
    sb.append("}");
    return sb.toString();
  }


}
