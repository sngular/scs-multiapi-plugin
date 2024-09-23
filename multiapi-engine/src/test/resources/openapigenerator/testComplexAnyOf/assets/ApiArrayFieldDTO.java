package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiArrayFieldDTO.ApiArrayFieldDTOBuilder.class)
public class ApiArrayFieldDTO {

  @JsonProperty(value ="uniqueItems")
  private Boolean uniqueItems;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private List<ApiTypeArrayDTO> defaultValues;
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="arraySize")
  private Integer arraySize;
  @JsonProperty(value ="regex")
  private String regex;
  @JsonProperty(value ="minItems")
  private Integer minItems;
  @JsonProperty(value ="values")
  private List<ApiTypeArrayDTO> values;

  private ApiArrayFieldDTO(ApiArrayFieldDTOBuilder builder) {
    this.uniqueItems = builder.uniqueItems;
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.arraySize = builder.arraySize;
    this.regex = builder.regex;
    this.minItems = builder.minItems;
    this.values = builder.values;

  }

  public static ApiArrayFieldDTO.ApiArrayFieldDTOBuilder builder() {
    return new ApiArrayFieldDTO.ApiArrayFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiArrayFieldDTOBuilder {

    private Boolean uniqueItems;
    private String type;
    private List<ApiTypeArrayDTO> defaultValues = new ArrayList<ApiTypeArrayDTO>();
    private String name;
    private Integer arraySize;
    private String regex;
    private Integer minItems;
    private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder uniqueItems(Boolean uniqueItems) {
      this.uniqueItems = uniqueItems;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder defaultValues(List<ApiTypeArrayDTO> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder defaultValue(ApiTypeArrayDTO defaultValue) {
      if (Objects.nonNull(defaultValue)) {
        this.defaultValues.add(defaultValue);
      }
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder arraySize(Integer arraySize) {
      this.arraySize = arraySize;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder regex(String regex) {
      this.regex = regex;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder minItems(Integer minItems) {
      this.minItems = minItems;
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder values(List<ApiTypeArrayDTO> values) {
      if (!values.isEmpty()) {
        this.values.addAll(values);
      }
      return this;
    }

    public ApiArrayFieldDTO.ApiArrayFieldDTOBuilder value(ApiTypeArrayDTO value) {
      if (Objects.nonNull(value)) {
        this.values.add(value);
      }
      return this;
    }

    public ApiArrayFieldDTO build() {
      ApiArrayFieldDTO apiArrayFieldDTO = new ApiArrayFieldDTO(this);
      return apiArrayFieldDTO;
    }
  }

  @Schema(name = "uniqueItems", required = false)
  public Boolean getUniqueItems() {
    return uniqueItems;
  }
  public void setUniqueItems(Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "defaultValues", required = false)
  public List<ApiTypeArrayDTO> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<ApiTypeArrayDTO> defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "arraySize", required = false)
  public Integer getArraySize() {
    return arraySize;
  }
  public void setArraySize(Integer arraySize) {
    this.arraySize = arraySize;
  }

  @Schema(name = "regex", required = false)
  public String getRegex() {
    return regex;
  }
  public void setRegex(String regex) {
    this.regex = regex;
  }

  @Schema(name = "minItems", required = false)
  public Integer getMinItems() {
    return minItems;
  }
  public void setMinItems(Integer minItems) {
    this.minItems = minItems;
  }

  @Schema(name = "values", required = false)
  public List<ApiTypeArrayDTO> getValues() {
    return values;
  }
  public void setValues(List<ApiTypeArrayDTO> values) {
    this.values = values;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiArrayFieldDTO apiArrayFieldDTO = (ApiArrayFieldDTO) o;
    return Objects.equals(this.uniqueItems, apiArrayFieldDTO.uniqueItems) && Objects.equals(this.type, apiArrayFieldDTO.type) && Objects.equals(this.defaultValues, apiArrayFieldDTO.defaultValues) && Objects.equals(this.name, apiArrayFieldDTO.name) && Objects.equals(this.arraySize, apiArrayFieldDTO.arraySize) && Objects.equals(this.regex, apiArrayFieldDTO.regex) && Objects.equals(this.minItems, apiArrayFieldDTO.minItems) && Objects.equals(this.values, apiArrayFieldDTO.values);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uniqueItems, type, defaultValues, name, arraySize, regex, minItems, values);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiArrayFieldDTO{");
    sb.append(" uniqueItems:").append(uniqueItems).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" arraySize:").append(arraySize).append(",");
    sb.append(" regex:").append(regex).append(",");
    sb.append(" minItems:").append(minItems).append(",");
    sb.append(" values:").append(values);
    sb.append("}");
    return sb.toString();
  }


}
