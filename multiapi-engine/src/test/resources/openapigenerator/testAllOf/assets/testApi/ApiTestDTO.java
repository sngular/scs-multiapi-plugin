package com.sngular.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testapi.model.exception.ModelClassException;
import com.sngular.multifileplugin.testapi.model.customvalidator.NotNull;

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

  public static class ApiTestDTOBuilder {

    private List<String> testers = new ArrayList<String>();
    private String testName;
    public ApiTestDTO.ApiTestDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder testers(String testers) {
      if (testers != null) {
        this.testers.add(testers);
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

  /**
  * Get testers
  * @return testers
  */
  @Schema(name = "testers", required = true)
  public List<String> getTesters() {
    return testers;
  }

  /**
  * Get testName
  * @return testName
  */
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
    sb.append("class ApiTestDTO {\n");
    sb.append(" testers: ").append(toIndentedString(testers)).append("\n");
    sb.append(" testName: ").append(toIndentedString(testName)).append("\n");
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
