package com.sngular.multifileplugin.inlineschemacreation.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiTestDTO.ApiTestDTOBuilder.class)
public class ApiTestDTO {

  @JsonProperty(value ="testProcessor")
  private ApiTestProcessorDTO testProcessor;
  @JsonProperty(value ="description")
  private String description;
  @JsonProperty(value ="tags")
  private List<String> tags = new ArrayList<String>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="testTypeList")
  private List<ApiTestTypeDTO> testTypeList = new ArrayList<ApiTestTypeDTO>();
  @JsonProperty(value ="id")
  private Integer id;
  @JsonProperty(value ="priority")
  private Integer priority;

  private ApiTestDTO(ApiTestProcessorDTO testProcessor, String description, List<String> tags, String name, List<ApiTestTypeDTO> testTypeList, Integer id, Integer priority) {
    this.testProcessor = testProcessor;
    this.description = description;
    this.tags = tags;
    this.name = name;
    this.testTypeList = testTypeList;
    this.id = id;
    this.priority = priority;

  }

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.testProcessor = builder.testProcessor;
    this.description = builder.description;
    this.tags = builder.tags;
    this.name = builder.name;
    this.testTypeList = builder.testTypeList;
    this.id = builder.id;
    this.priority = builder.priority;

  }

  public static ApiTestDTO.ApiTestDTOBuilder builder() {
    return new ApiTestDTO.ApiTestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTestDTOBuilder {

    private ApiTestProcessorDTO testProcessor;
    private String description;
    private List<String> tags = new ArrayList<String>();
    private String name;
    private List<ApiTestTypeDTO> testTypeList = new ArrayList<ApiTestTypeDTO>();
    private Integer id;
    private Integer priority;
    public ApiTestDTO.ApiTestDTOBuilder testProcessor(ApiTestProcessorDTO testProcessor) {
      this.testProcessor = testProcessor;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder description(String description) {
      this.description = description;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder tags(List<String> tags) {
      if (!tags.isEmpty()) {
        this.tags.addAll(tags);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder tags(String tags) {
      if (tags != null) {
        this.tags.add(tags);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder name(String name) {
      this.name = name;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder testTypeList(List<ApiTestTypeDTO> testTypeList) {
      if (!testTypeList.isEmpty()) {
        this.testTypeList.addAll(testTypeList);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder testTypeList(ApiTestTypeDTO testTypeList) {
      if (testTypeList != null) {
        this.testTypeList.add(testTypeList);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder priority(Integer priority) {
      this.priority = priority;
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO = new ApiTestDTO(this);
      return apiTestDTO;
    }
  }

  /**
  * Get testProcessor
  * @return testProcessor
  */
  @Schema(name = "testProcessor", required = false)
  public ApiTestProcessorDTO getTestProcessor() {
    return testProcessor;
  }
  public void setTestProcessor(ApiTestProcessorDTO testProcessor) {
    this.testProcessor = testProcessor;
  }

  /**
  * Get description
  * @return description
  */
  @Schema(name = "description", required = false)
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  /**
  * Get tags
  * @return tags
  */
  @Schema(name = "tags", required = false)
  public List<String> getTags() {
    return tags;
  }
  public void setTags(List<String> tags) {
    this.tags = tags;
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
  * Get testTypeList
  * @return testTypeList
  */
  @Schema(name = "testTypeList", required = false)
  public List<ApiTestTypeDTO> getTestTypeList() {
    return testTypeList;
  }
  public void setTestTypeList(List<ApiTestTypeDTO> testTypeList) {
    this.testTypeList = testTypeList;
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
    this.id = id;
  }

  /**
  * Get priority
  * @return priority
  */
  @Schema(name = "priority", required = false)
  public Integer getPriority() {
    return priority;
  }
  public void setPriority(Integer priority) {
    this.priority = priority;
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
    return Objects.equals(this.testProcessor, apiTestDTO.testProcessor) && Objects.equals(this.description, apiTestDTO.description) && Objects.equals(this.tags, apiTestDTO.tags) && Objects.equals(this.name, apiTestDTO.name) && Objects.equals(this.testTypeList, apiTestDTO.testTypeList) && Objects.equals(this.id, apiTestDTO.id) && Objects.equals(this.priority, apiTestDTO.priority);
  }

  @Override
  public int hashCode() {
    return Objects.hash(testProcessor, description, tags, name, testTypeList, id, priority);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestDTO {\n");
    sb.append(" testProcessor: ").append(toIndentedString(testProcessor)).append("\n");
    sb.append(" description: ").append(toIndentedString(description)).append("\n");
    sb.append(" tags: ").append(toIndentedString(tags)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" testTypeList: ").append(toIndentedString(testTypeList)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" priority: ").append(toIndentedString(priority)).append("\n");
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
