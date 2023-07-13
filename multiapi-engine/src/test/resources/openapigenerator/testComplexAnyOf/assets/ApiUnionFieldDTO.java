package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ApiUnionFieldDTO.ApiUnionFieldDTOBuilder.class)
public class ApiUnionFieldDTO {

  @JsonProperty(value ="defaultItem")
  private ApiDefaultItemDTO defaultItem;
  @JsonProperty(value ="generatedFlag")
  private Boolean generatedFlag;
  @JsonProperty(value ="type")
  private String type;
  @JsonProperty(value ="unionEnum")
  private UnionEnum unionEnum;
  public enum UnionEnum {
    ONEOF("oneof"),
    ANYOF("anyof"),
    ALLOF("allof");

    private String value;

    UnionEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="optionalUnion")
  private Boolean optionalUnion;
  @JsonProperty(value ="values")
  private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();

  private ApiUnionFieldDTO(ApiDefaultItemDTO defaultItem, Boolean generatedFlag, String type, UnionEnum unionEnum, String name, Boolean optionalUnion, List<ApiTypeArrayDTO> values) {
    this.defaultItem = defaultItem;
    this.generatedFlag = generatedFlag;
    this.type = type;
    this.unionEnum = unionEnum;
    this.name = name;
    this.optionalUnion = optionalUnion;
    this.values = values;

  }

  private ApiUnionFieldDTO(ApiUnionFieldDTOBuilder builder) {
    this.defaultItem = builder.defaultItem;
    this.generatedFlag = builder.generatedFlag;
    this.type = builder.type;
    this.unionEnum = builder.unionEnum;
    this.name = builder.name;
    this.optionalUnion = builder.optionalUnion;
    this.values = builder.values;

  }

  public static ApiUnionFieldDTO.ApiUnionFieldDTOBuilder builder() {
    return new ApiUnionFieldDTO.ApiUnionFieldDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiUnionFieldDTOBuilder {

    private ApiDefaultItemDTO defaultItem;
    private Boolean generatedFlag;
    private String type;
    private UnionEnum unionEnum;
    private String name;
    private Boolean optionalUnion;
    private List<ApiTypeArrayDTO> values = new ArrayList<ApiTypeArrayDTO>();

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder defaultItem(ApiDefaultItemDTO defaultItem) {
      this.defaultItem = defaultItem;
      return this;
    }

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder generatedFlag(Boolean generatedFlag) {
      this.generatedFlag = generatedFlag;
      return this;
    }

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder unionEnum(UnionEnum unionEnum) {
      this.unionEnum = unionEnum;
      return this;
    }

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder optionalUnion(Boolean optionalUnion) {
      this.optionalUnion = optionalUnion;
      return this;
    }
    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder values(List<ApiTypeArrayDTO> values) {
      if (!values.isEmpty()) {
        this.values.addAll(values);
      }
      return this;
    }

    public ApiUnionFieldDTO.ApiUnionFieldDTOBuilder values(ApiTypeArrayDTO values) {
      if (values != null) {
        this.values.add(values);
      }
      return this;
    }

    public ApiUnionFieldDTO build() {
      ApiUnionFieldDTO apiUnionFieldDTO = new ApiUnionFieldDTO(this);
      return apiUnionFieldDTO;
    }
  }

  @Schema(name = "defaultItem", required = false)
  public ApiDefaultItemDTO getDefaultItem() {
    return defaultItem;
  }
  public void setDefaultItem(ApiDefaultItemDTO defaultItem) {
    this.defaultItem = defaultItem;
  }

  @Schema(name = "generatedFlag", required = false)
  public Boolean getGeneratedFlag() {
    return generatedFlag;
  }
  public void setGeneratedFlag(Boolean generatedFlag) {
    this.generatedFlag = generatedFlag;
  }

  @Schema(name = "type", required = false)
  public String getType() {
    return type;
  }
  public void setType(String type) {
    this.type = type;
  }

  @Schema(name = "unionEnum", required = false)
  public UnionEnum getUnionEnum() {
    return unionEnum;
  }
  public void setUnionEnum(UnionEnum unionEnum) {
    this.unionEnum = unionEnum;
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "optionalUnion", required = false)
  public Boolean getOptionalUnion() {
    return optionalUnion;
  }
  public void setOptionalUnion(Boolean optionalUnion) {
    this.optionalUnion = optionalUnion;
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
    ApiUnionFieldDTO apiUnionFieldDTO = (ApiUnionFieldDTO) o;
    return Objects.equals(this.defaultItem, apiUnionFieldDTO.defaultItem) && Objects.equals(this.generatedFlag, apiUnionFieldDTO.generatedFlag) && Objects.equals(this.type, apiUnionFieldDTO.type) && Objects.equals(this.unionEnum, apiUnionFieldDTO.unionEnum) && Objects.equals(this.name, apiUnionFieldDTO.name) && Objects.equals(this.optionalUnion, apiUnionFieldDTO.optionalUnion) && Objects.equals(this.values, apiUnionFieldDTO.values);
  }

  @Override
  public int hashCode() {
    return Objects.hash(defaultItem, generatedFlag, type, unionEnum, name, optionalUnion, values);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiUnionFieldDTO{");
    sb.append(" defaultItem:").append(defaultItem).append(",");
    sb.append(" generatedFlag:").append(generatedFlag).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" unionEnum:").append(unionEnum).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" optionalUnion:").append(optionalUnion).append(",");
    sb.append(" values:").append(values).append(",");
    sb.append("}");
    return sb.toString();
  }


}
