package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiStringFieldDTO.ApiStringFieldDTOBuilder.class)
public class ApiStringFieldDTO {

  @JsonProperty(value ="maxLength")
  private Integer maxLength;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="properties")
  private List<String> properties = new ArrayList<String>();
  @JsonProperty(value ="defaultValues")
  private List<String> defaultValues = new ArrayList<String>();
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="regex")
  private String regex;
  @JsonProperty(value ="minLength")
  private Integer minLength;
  @JsonProperty(value ="format")
  private Integer format;
  @JsonProperty(value ="valueLength")
  private Integer valueLength;

  private ApiStringFieldDTO(Integer maxLength, String type, List<String> properties, List<String> defaultValues, String name, String regex, Integer minLength, Integer format, Integer valueLength) {
    this.maxLength = maxLength;
    this.type = type;
    this.properties = properties;
    this.defaultValues = defaultValues;
    this.name = name;
    this.regex = regex;
    this.minLength = minLength;
    this.format = format;
    this.valueLength = valueLength;

  }

  private ApiStringFieldDTO(ApiStringFieldDTOBuilder builder) {
    this.maxLength = builder.maxLength;
    this.type = builder.type;
    this.properties = builder.properties;
    this.defaultValues = builder.defaultValues;
    this.name = builder.name;
    this.regex = builder.regex;
    this.minLength = builder.minLength;
    this.format = builder.format;
    this.valueLength = builder.valueLength;

  }

  public static ApiStringFieldDTO.ApiStringFieldDTOBuilder builder() {
    return new ApiStringFieldDTO.ApiStringFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiStringFieldDTOBuilder {

    private Integer maxLength;
    private String type;
    private List<String> properties = new ArrayList<String>();
    private List<String> defaultValues = new ArrayList<String>();
    private String name;
    private String regex;
    private Integer minLength;
    private Integer format;
    private Integer valueLength;

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder maxLength(Integer maxLength) {
      this.maxLength = maxLength;
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiStringFieldDTO.ApiStringFieldDTOBuilder properties(List<String> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder propertie(String propertie) {
      if (propertie != null) {
        this.properties.add(propertie);
      }
      return this;
    }
    public ApiStringFieldDTO.ApiStringFieldDTOBuilder defaultValues(List<String> defaultValues) {
      if (!defaultValues.isEmpty()) {
        this.defaultValues.addAll(defaultValues);
      }
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder defaultValue(String defaultValue) {
      if (defaultValue != null) {
        this.defaultValues.add(defaultValue);
      }
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder regex(String regex) {
      this.regex = regex;
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder minLength(Integer minLength) {
      this.minLength = minLength;
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder format(Integer format) {
      this.format = format;
      return this;
    }

    public ApiStringFieldDTO.ApiStringFieldDTOBuilder valueLength(Integer valueLength) {
      this.valueLength = valueLength;
      return this;
    }

    public ApiStringFieldDTO build() {
      ApiStringFieldDTO apiStringFieldDTO = new ApiStringFieldDTO(this);
      return apiStringFieldDTO;
    }
  }

  @Schema(name = "maxLength", required = false)
  public Integer getMaxLength() {
    return maxLength;
  }
  public void setMaxLength(Integer maxLength) {
    this.maxLength = maxLength;
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "properties", required = false)
  public List<String> getProperties() {
    return properties;
  }
  public void setProperties(List<String> properties) {
    this.properties = properties;
  }

  @Schema(name = "defaultValues", required = false)
  public List<String> getDefaultValues() {
    return defaultValues;
  }
  public void setDefaultValues(List<String> defaultValues) {
    this.defaultValues = defaultValues;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "regex", required = false)
  public String getRegex() {
    return regex;
  }
  public void setRegex(String regex) {
    this.regex = regex;
  }

  @Schema(name = "minLength", required = false)
  public Integer getMinLength() {
    return minLength;
  }
  public void setMinLength(Integer minLength) {
    this.minLength = minLength;
  }

  @Schema(name = "format", required = false)
  public Integer getFormat() {
    return format;
  }
  public void setFormat(Integer format) {
    this.format = format;
  }

  @Schema(name = "valueLength", required = false)
  public Integer getValueLength() {
    return valueLength;
  }
  public void setValueLength(Integer valueLength) {
    this.valueLength = valueLength;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiStringFieldDTO apiStringFieldDTO = (ApiStringFieldDTO) o;
    return Objects.equals(this.maxLength, apiStringFieldDTO.maxLength) && Objects.equals(this.type, apiStringFieldDTO.type) && Objects.equals(this.properties, apiStringFieldDTO.properties) && Objects.equals(this.defaultValues, apiStringFieldDTO.defaultValues) && Objects.equals(this.name, apiStringFieldDTO.name) && Objects.equals(this.regex, apiStringFieldDTO.regex) && Objects.equals(this.minLength, apiStringFieldDTO.minLength) && Objects.equals(this.format, apiStringFieldDTO.format) && Objects.equals(this.valueLength, apiStringFieldDTO.valueLength);
  }

  @Override
  public int hashCode() {
    return Objects.hash(maxLength, type, properties, defaultValues, name, regex, minLength, format, valueLength);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiStringFieldDTO{");
    sb.append(" maxLength:").append(maxLength).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" properties:").append(properties).append(",");
    sb.append(" defaultValues:").append(defaultValues).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" regex:").append(regex).append(",");
    sb.append(" minLength:").append(minLength).append(",");
    sb.append(" format:").append(format).append(",");
    sb.append(" valueLength:").append(valueLength);
    sb.append("}");
    return sb.toString();
  }


}
