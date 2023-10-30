package com.sngular.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testapi.model.exception.ModelClassException;
import com.sngular.multifileplugin.testapi.model.customvalidator.NotNull;

@JsonDeserialize(builder = ApiTestDTO.ApiTestDTOBuilder.class)
public class ApiTestDTO {

  @JsonProperty(value ="testers")
  @NotNull
  private final List<String> testers;
  @JsonProperty(value ="testName")
  @NotNull
  private final String testName;

  private ApiTestDTO(List<String> testers, String testName) {
    this.testers = testers;
    this.testName = testName;

    validateRequiredAttributes();
  }

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.testers = builder.testers;
    this.testName = builder.testName;

    validateRequiredAttributes();
  }

  public static ApiTestDTO.ApiTestDTOBuilder builder() {
    return new ApiTestDTO.ApiTestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestDTOBuilder {

    private List<String> testers = new ArrayList<String>();
    private String testName;
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

    public ApiTestDTO.ApiTestDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO = new ApiTestDTO(this);
      return apiTestDTO;
    }
  }

  @Schema(name = "testers", required = true)
  public List<String> getTesters() {
    return testers;
  }

  @Schema(name = "testName", required = true)
  public String getTestName() {
    return testName;
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
    return Objects.equals(this.testers, apiTestDTO.testers) && Objects.equals(this.testName, apiTestDTO.testName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testers, testName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestDTO{");
    sb.append(" testers:").append(testers).append(",");
    sb.append(" testName:").append(testName);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.testers)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.testName)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTestDTO");
    }
  }

}
