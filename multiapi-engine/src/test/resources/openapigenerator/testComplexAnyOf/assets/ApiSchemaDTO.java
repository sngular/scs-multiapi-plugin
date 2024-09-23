package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testcomplexanyof.model.ApiTypeArrayDTO;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testcomplexanyof.model.exception.ModelClassException;
import com.sngular.multifileplugin.testcomplexanyof.model.customvalidator.NotNull;

@JsonDeserialize(builder = ApiSchemaDTO.ApiSchemaDTOBuilder.class)
public class ApiSchemaDTO {

  @JsonProperty(value ="type")
  @NotNull
  private final String type;
  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="id")
  @NotNull
  private final String id;
  @JsonProperty(value ="subjectName")
  @NotNull
  private final String subjectName;
  @JsonProperty(value ="requiredFields")
  private List<ApiTypeArrayDTO> requiredFields;

  private ApiSchemaDTO(ApiSchemaDTOBuilder builder) {
    this.type = builder.type;
    this.name = builder.name;
    this.id = builder.id;
    this.subjectName = builder.subjectName;
    this.requiredFields = builder.requiredFields;

    validateRequiredAttributes();
  }

  public static ApiSchemaDTO.ApiSchemaDTOBuilder builder() {
    return new ApiSchemaDTO.ApiSchemaDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiSchemaDTOBuilder {

    private String type;
    private String name;
    private String id;
    private String subjectName;
    private List<ApiTypeArrayDTO> requiredFields = new ArrayList<ApiTypeArrayDTO>();

    public ApiSchemaDTO.ApiSchemaDTOBuilder type(String type) {
      this.type = type;
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder subjectName(String subjectName) {
      this.subjectName = subjectName;
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder requiredFields(List<ApiTypeArrayDTO> requiredFields) {
      if (!requiredFields.isEmpty()) {
        this.requiredFields.addAll(requiredFields);
      }
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder requiredField(ApiTypeArrayDTO requiredField) {
      if (Objects.nonNull(requiredField)) {
        this.requiredFields.add(requiredField);
      }
      return this;
    }

    public ApiSchemaDTO build() {
      ApiSchemaDTO apiSchemaDTO = new ApiSchemaDTO(this);
      return apiSchemaDTO;
    }
  }

  @Schema(name = "type", required = true)
  public String getType() {
    return type;
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Schema(name = "id", required = true)
  public String getId() {
    return id;
  }

  @Schema(name = "subjectName", required = true)
  public String getSubjectName() {
    return subjectName;
  }

  @Schema(name = "requiredFields", required = false)
  public List<ApiTypeArrayDTO> getRequiredFields() {
    return requiredFields;
  }
  public void setRequiredFields(List<ApiTypeArrayDTO> requiredFields) {
    this.requiredFields = requiredFields;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiSchemaDTO apiSchemaDTO = (ApiSchemaDTO) o;
    return Objects.equals(this.type, apiSchemaDTO.type) && Objects.equals(this.name, apiSchemaDTO.name) && Objects.equals(this.id, apiSchemaDTO.id) && Objects.equals(this.subjectName, apiSchemaDTO.subjectName) && Objects.equals(this.requiredFields, apiSchemaDTO.requiredFields);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, name, id, subjectName, requiredFields);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiSchemaDTO{");
    sb.append(" type:").append(type).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append(" subjectName:").append(subjectName).append(",");
    sb.append(" requiredFields:").append(requiredFields);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.type)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.id)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.subjectName)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiSchemaDTO");
    }
  }

}
