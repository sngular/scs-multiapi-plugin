package net.coru.multifileplugin.tagsgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import net.coru.multifileplugin.tagsgeneration.model.exception.ModelClassException;

public class TestInfoDTO {

  @JsonProperty(value ="testName")
  private final String testName;
  @JsonProperty(value ="testers")
  private final List<String> testers;

  private TestInfoDTO(String testName, List<String> testers) {
    this.testName = testName;
    this.testers = testers;

    validateRequiredAttributes();
  }

  private TestInfoDTO(TestInfoDTOBuilder builder) {
    this.testName = builder.testName;
    this.testers = builder.testers;

    validateRequiredAttributes();
  }

  public static class TestInfoDTOBuilder {

    private String testName;
    private List<String> testers = new ArrayList<String>();

    public TestInfoDTO.TestInfoDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }
    public TestInfoDTO.TestInfoDTOBuilder testers(List<String> testers) {
      if (!testers.isEmpty()) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public TestInfoDTO.TestInfoDTOBuilder tester(String tester) {
      if (tester != null) {
        this.testers.add(tester);
      }
      return this;
    }

    public TestInfoDTO build() {
      TestInfoDTO testInfoDTO = new TestInfoDTO(this);
      return testInfoDTO;
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
    TestInfoDTO testInfoDTO = (TestInfoDTO) o;
    return Objects.equals(this.testName, testInfoDTO.testName) && Objects.equals(this.testers, testInfoDTO.testers);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testName, testers);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestInfoDTO {\n");
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
      throw new ModelClassException("TestInfoDTO");
    }
  }

}
