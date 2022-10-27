package net.coru.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.multifileplugin.testcomplexanyof.model.ApiTypeArrayDTO;
import java.util.List;
import java.util.ArrayList;
import net.coru.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

public class ApiArrayFieldDTO {

  @JsonProperty(value ="uniqueItems")
  private Boolean uniqueItems;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="defaultValues")
  private List<ApiTypeArrayDTO> defaultValues = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="arraySize")
  private Integer arraySize;
  @JsonProperty(value ="regex")
  private String regex;
  @JsonProperty(value ="minItems")
  private Integer minItems;
  @JsonProperty(value ="values")
  private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();

  private ApiArrayFieldDTO(Boolean uniqueItems, String type, List<ApiTypeArrayDTO> defaultValues, String name, Integer arraySize, String regex, Integer minItems, List<ApiTypeArrayDTO> values) {
    this.uniqueItems = uniqueItems;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.arraySize = arraySize;
    this.regex = regex;
    this.minItems = minItems;
    this.values = values;

    validatePartialCombinations();
  }

  private ApiArrayFieldDTO(ApiArrayFieldDTOBuilder builder) {
    this.uniqueItems = builder.uniqueItems;
    this.type = builder.type;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.arraySize = builder.arraySize;
    this.regex = builder.regex;
    this.minItems = builder.minItems;
    this.values = builder.values;

    validatePartialCombinations();
  }

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
      if (defaultValue != null) {
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
      if (value != null) {
        this.values.add(value);
      }
      return this;
    }

    public ApiArrayFieldDTO build() {
      ApiArrayFieldDTO apiArrayFieldDTO = new ApiArrayFieldDTO(this);
      return apiArrayFieldDTO;
    }
  }

  /**
  * Get uniqueItems
  * @return uniqueItems
  */
  @Schema(name = "uniqueItems", required = false)
  public Boolean getUniqueItems() {
    return uniqueItems;
  }
  public void setUniqueItems(Boolean uniqueItems) {
    this.uniqueItems = uniqueItems;
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
  * Get defaultValues
  * @return defaultValues
  */
  @Schema(name = "defaultValues", required = false)
  public List<ApiTypeArrayDTO> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<ApiTypeArrayDTO> defaultValues) {
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

  /**
  * Get arraySize
  * @return arraySize
  */
  @Schema(name = "arraySize", required = false)
  public Integer getArraySize() {
    return arraySize;
  }
  public void setArraySize(Integer arraySize) {
    this.arraySize = arraySize;
  }

  /**
  * Get regex
  * @return regex
  */
  @Schema(name = "regex", required = false)
  public String getRegex() {
    return regex;
  }
  public void setRegex(String regex) {
    this.regex = regex;
  }

  /**
  * Get minItems
  * @return minItems
  */
  @Schema(name = "minItems", required = false)
  public Integer getMinItems() {
    return minItems;
  }
  public void setMinItems(Integer minItems) {
    this.minItems = minItems;
  }

  /**
  * Get values
  * @return values
  */
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
    sb.append("class ApiArrayFieldDTO {\n");
    sb.append(" uniqueItems: ").append(toIndentedString(uniqueItems)).append("\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" defaultValues: ").append(toIndentedString(defaultValues)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" arraySize: ").append(toIndentedString(arraySize)).append("\n");
    sb.append(" regex: ").append(toIndentedString(regex)).append("\n");
    sb.append(" minItems: ").append(toIndentedString(minItems)).append("\n");
    sb.append(" values: ").append(toIndentedString(values)).append("\n");
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

    if (Objects.nonNull(this.uniqueItems)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.type)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.defaultValues)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.arraySize)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.regex)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.minItems)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.values)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiArrayFieldDTO");
    }
  }


}
