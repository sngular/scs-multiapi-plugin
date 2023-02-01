package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testcomplexanyof.model.ApiTypeArrayDTO;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

public class ApiObjectFieldDTO {

  @JsonProperty(value ="requiredValues")
  private List<Object> requiredValues = new ArrayList<Object>();
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="properties")
  private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();
  @JsonProperty(value ="name")
  private String name;

  private ApiObjectFieldDTO(List<Object> requiredValues, String type, List<ApiTypeArrayDTO> properties, List<Object> defaultValues, String name) {
    this.requiredValues = requiredValues;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.name = name;

    validatePartialCombinations();
  }

  private ApiObjectFieldDTO(ApiObjectFieldDTOBuilder builder) {
    this.requiredValues = builder.requiredValues;
    this.type = builder.type;
    this.properties = builder.properties;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;

    validatePartialCombinations();
  }

  public static ApiObjectFieldDTO.ApiObjectFieldDTOBuilder builder() {
    return new ApiObjectFieldDTO.ApiObjectFieldDTOBuilder();
  }

  public static class ApiObjectFieldDTOBuilder {

    private List<Object> requiredValues = new ArrayList<Object>();
    private String type;
    private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
    private List<Object> defaultValues = new ArrayList<Object>();
    private String name;
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder requiredValues(List<Object> requiredValues) {
      if (!requiredValues.isEmpty()) {
        this.requiredValues.addAll(requiredValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder requiredValues(Object requiredValues) {
      if (requiredValues != null) {
        this.requiredValues.add(requiredValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder properties(List<ApiTypeArrayDTO> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder properties(ApiTypeArrayDTO properties) {
      if (properties != null) {
        this.properties.add(properties);
      }
      return this;
    }
    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder defaultValues(List<Object> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder defaultValues(Object defaultValues) {
      if (defaultValues != null) {
        this.defaultValues.add(defaultValues);
      }
      return this;
    }

    public ApiObjectFieldDTO.ApiObjectFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiObjectFieldDTO build() {
      ApiObjectFieldDTO apiObjectFieldDTO = new ApiObjectFieldDTO(this);
      return apiObjectFieldDTO;
    }
  }

  /**
  * Get requiredValues
  * @return requiredValues
  */
  @Schema(name = "requiredValues", required = false)
  public List<Object> getRequiredValues() {
    return requiredValues;
  }
  public void setRequiredValues(List<Object> requiredValues) {
    this.requiredValues = requiredValues;
  }

  /**
  * Get type
  * @return type
  */
  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  /**
  * Get properties
  * @return properties
  */
  @Schema(name = "properties", required = false)
  public List<ApiTypeArrayDTO> getProperties() {
    return properties;
  }
  public void setProperties(List<ApiTypeArrayDTO> properties) {
    this.properties = properties;
  }

  /**
  * Get defaultValues
  * @return defaultValues
  */
  @Schema(name = "defaultValues", required = false)
  public List<Object> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<Object> defaultValues) {
    this.defaultValues = defaultValues;
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiObjectFieldDTO apiObjectFieldDTO = (ApiObjectFieldDTO) o;
    return Objects.equals(this.requiredValues, apiObjectFieldDTO.requiredValues) && Objects.equals(this.type, apiObjectFieldDTO.type) && Objects.equals(this.properties, apiObjectFieldDTO.properties) && Objects.equals(this.defaultValues, apiObjectFieldDTO.defaultValues) && Objects.equals(this.name, apiObjectFieldDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(requiredValues, type, properties, defaultValues, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiObjectFieldDTO {\n");
    sb.append(" requiredValues: ").append(toIndentedString(requiredValues)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" properties: ").append(toIndentedString(properties)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
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

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.requiredValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.type)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.properties)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.defaultValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiObjectFieldDTO");
    }
  }


}
