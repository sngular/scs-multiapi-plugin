package com.sngular.multifileplugin.pathparameter.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.pathparameter.model.exception.ModelClassException;
import com.sngular.multifileplugin.pathparameter.model.customvalidator.NotNull;

@JsonDeserialize(builder = TestInfoDTO.TestInfoDTOBuilder.class)
public class TestInfoDTO {

  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();
  @JsonProperty(value ="testName")
  @NotNull
  private final String testName;

  private TestInfoDTO(List<String> testers, String testName) {
    this.testers = testers;
    this.testName = testName;

    validateRequiredAttributes();
  }

  private TestInfoDTO(TestInfoDTOBuilder builder) {
    this.testers = builder.testers;
    this.testName = builder.testName;

    validateRequiredAttributes();
  }

  public static TestInfoDTO.TestInfoDTOBuilder builder() {
    return new TestInfoDTO.TestInfoDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TestInfoDTOBuilder {

    private List<String> testers = new ArrayList<String>();
    private String testName;
    public TestInfoDTO.TestInfoDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public TestInfoDTO.TestInfoDTOBuilder testers(String testers) {
      if (testers != null) {
        this.testers.add(testers);
      }
      return this;
    }

    public TestInfoDTO.TestInfoDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }

    public TestInfoDTO build() {
      TestInfoDTO testInfoDTO = new TestInfoDTO(this);
      return testInfoDTO;
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
    TestInfoDTO testInfoDTO = (TestInfoDTO) o;
    return Objects.equals(this.testers, testInfoDTO.testers) && Objects.equals(this.testName, testInfoDTO.testName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testers, testName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestInfoDTO {\n");
    sb.append(" testers: ").append(toIndentedString(testers)).append("\n");
    sb.append(" testName: ").append(toIndentedString(testName)).append("\n");
    sb.append("}");
    return sb.toString();
  }

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

    if (!satisfiedCondition) {
      throw new ModelClassException("TestInfoDTO");
    }
  }

}
