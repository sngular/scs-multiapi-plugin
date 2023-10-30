package com.sngular.multifileplugin.testoverwriteapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testoverwriteapi.model.exception.ModelClassException;
import com.sngular.multifileplugin.testoverwriteapi.model.customvalidator.NotNull;

@JsonDeserialize(builder = ApiTestInfoDTO.ApiTestInfoDTOBuilder.class)
public class ApiTestInfoDTO {

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();
  @JsonProperty(value ="testName")
  @NotNull
  private final String testName;

  private ApiTestInfoDTO(List<String> testers, String testName) {
    this.testers = testers;
    this.testName = testName;

    validateRequiredAttributes();
  }

  private ApiTestInfoDTO(ApiTestInfoDTOBuilder builder) {
    this.testers = builder.testers;
    this.testName = builder.testName;

    validateRequiredAttributes();
  }

  public static ApiTestInfoDTO.ApiTestInfoDTOBuilder builder() {
    return new ApiTestInfoDTO.ApiTestInfoDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestInfoDTOBuilder {

    private List<String> testers = new ArrayList<String>();
    private String testName;
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

    public ApiTestInfoDTO.ApiTestInfoDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }

    public ApiTestInfoDTO build() {
      ApiTestInfoDTO apiTestInfoDTO = new ApiTestInfoDTO(this);
      return apiTestInfoDTO;
    }
  }

  @Schema(name = "testers", required = false)
  public List<String> getTesters() {
    return testers;
  }
  public void setTesters(List<String> testers) {
    this.testers = testers;
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
    ApiTestInfoDTO apiTestInfoDTO = (ApiTestInfoDTO) o;
    return Objects.equals(this.testers, apiTestInfoDTO.testers) && Objects.equals(this.testName, apiTestInfoDTO.testName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testers, testName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiTestInfoDTO{");
    sb.append(" testers:").append(testers).append(",");
    sb.append(" testName:").append(testName);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.testName)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTestInfoDTO");
    }
  }

}
