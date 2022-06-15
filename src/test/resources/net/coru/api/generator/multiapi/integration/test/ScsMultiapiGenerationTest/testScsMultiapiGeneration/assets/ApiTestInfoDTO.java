package net.coru.multifileplugin.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.collections4.CollectionUtils;
import java.util.List;
import java.util.ArrayList;

public class ApiTestInfoDTO {

  @JsonProperty(value ="testName")
  private String testName;
  @JsonProperty(value ="testers")
  private List<String> testers = new ArrayList<String>();

  private ApiTestInfoDTO(ApiTestInfoDTOBuilder builder) {
    this.testName = builder.testName;
    this.testers = builder.testers;
  }

  public static class ApiTestInfoDTOBuilder {

    private String testName;
    private List<String> testers = new ArrayList<String>();

    public ApiTestInfoDTO.ApiTestInfoDTOBuilder testName(String testName) {
      this.testName = testName;
      return this;
    }
    public ApiTestInfoDTO.ApiTestInfoDTOBuilder testers(List<String> testers) {
      if (CollectionUtils.isNotEmpty(testers)) {
        this.testers.addAll(testers);
      }
      return this;
    }

    public ApiTestInfoDTO.ApiTestInfoDTOBuilder tester(String tester) {
      if (Objects.nonNull(tester)) {
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
  @ApiModelProperty( value = "description")
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
  @ApiModelProperty( value = "description")
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

}
