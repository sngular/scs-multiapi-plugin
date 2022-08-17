package net.coru.multifileplugin.inlineschemacreation.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.multifileplugin.inlineschemacreation.model.ApiTestTypeDTO;
import java.util.List;
import java.util.ArrayList;


public class ApiTestDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="description")
  private String description;
  @JsonProperty(value ="testTypeList")
  private List<ApiTestTypeDTO> testTypeList = new ArrayList<ApiTestTypeDTO>();
  @JsonProperty(value ="testProcessor")
  private ApiTestProcessorDTO testProcessor;
  @JsonProperty(value ="id")
  private Integer id;
  @JsonProperty(value ="priority")
  private Integer priority;
  @JsonProperty(value ="tags")
  private List<String> tags = new ArrayList<String>();

  private ApiTestDTO(String name, String description, List<ApiTestTypeDTO> testTypeList, ApiTestProcessorDTO testProcessor, Integer id, Integer priority, List<String> tags){
    this.name = name;
    this.description = description;
    this.testTypeList = testTypeList;
    this.testProcessor = testProcessor;
    this.id = id;
    this.priority = priority;
    this.tags = tags;

  }

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.name = builder.name;
    this.description = builder.description;
    this.testTypeList = builder.testTypeList;
    this.testProcessor = builder.testProcessor;
    this.id = builder.id;
    this.priority = builder.priority;
    this.tags = builder.tags;

  }

  public static class ApiTestDTOBuilder {

    private String name;
    private String description;
    private List<ApiTestTypeDTO> testTypeList = new ArrayList<ApiTestTypeDTO>();
    private ApiTestProcessorDTO testProcessor;
    private Integer id;
    private Integer priority;
    private List<String> tags = new ArrayList<String>();

    public ApiTestDTO.ApiTestDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder description(String description) {
      this.description = description;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder testTypeList(List<ApiTestTypeDTO> testTypeList) {
      if (!testTypeList.isEmpty()) {
        this.testTypeList.addAll(testTypeList);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder testTypeLis(ApiTestTypeDTO testTypeLis) {
      if (testTypeLis != null) {
        this.testTypeList.add(testTypeLis);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder testProcessor(ApiTestProcessorDTO testProcessor) {
      this.testProcessor = testProcessor;
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
    public ApiTestDTO.ApiTestDTOBuilder tags(List<String> tags) {
      if (!tags.isEmpty()) {
        this.tags.addAll(tags);
      }
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder tag(String tag) {
      if (tag != null) {
        this.tags.add(tag);
      }
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO =  new ApiTestDTO(this);
      return apiTestDTO;
    }
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestDTO apiTestDTO = (ApiTestDTO) o;
    return Objects.equals(this.name,apiTestDTO.name) && Objects.equals(this.description,apiTestDTO.description) && Objects.equals(this.testTypeList,apiTestDTO.testTypeList) && Objects.equals(this.testProcessor,apiTestDTO.testProcessor) && Objects.equals(this.id,apiTestDTO.id) && Objects.equals(this.priority,apiTestDTO.priority) && Objects.equals(this.tags,apiTestDTO.tags) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(name,description,testTypeList,testProcessor,id,priority,tags);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestDTO {\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" description: ").append(toIndentedString(description)).append("\n");
    sb.append(" testTypeList: ").append(toIndentedString(testTypeList)).append("\n");
    sb.append(" testProcessor: ").append(toIndentedString(testProcessor)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" priority: ").append(toIndentedString(priority)).append("\n");
    sb.append(" tags: ").append(toIndentedString(tags)).append("\n");
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
