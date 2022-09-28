package net.coru.multifileplugin.pathparameter.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class TestsDTO {

  @JsonProperty(value ="testDTO")
  private TestDTO testDTO;

  private TestsDTO(TestDTO testDTO) {
    this.testDTO = testDTO;

  }

  private TestsDTO(TestsDTOBuilder builder) {
    this.testDTO = builder.testDTO;

  }

  public static class TestsDTOBuilder {

    private TestDTO testDTO;

    public TestsDTO.TestsDTOBuilder testDTO(TestDTO testDTO) {
      this.testDTO = testDTO;
      return this;
    }

    public TestsDTO build() {
      TestsDTO testsDTO = new TestsDTO(this);
      return testsDTO;
    }
  }

  /**
  * Get testDTO
  * @return testDTO
  */
  @Schema(name = "testDTO", required = false)
  public TestDTO getTestDTO() {
    return testDTO;
  }
  public void setTestDTO(TestDTO testDTO) {
    this.testDTO = testDTO;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TestsDTO testsDTO = (TestsDTO) o;
    return Objects.equals(this.testDTO, testsDTO.testDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestsDTO {\n");
    sb.append(" testDTO: ").append(toIndentedString(testDTO)).append("\n");
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
