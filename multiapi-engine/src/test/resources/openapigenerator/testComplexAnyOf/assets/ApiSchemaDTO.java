package net.coru.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.multifileplugin.testcomplexanyof.model.ApiTypeArrayDTO;
import java.util.List;
import java.util.ArrayList;
import net.coru.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

public class ApiSchemaDTO {

  @JsonProperty(value ="type")
  private final String type;
  @JsonProperty(value ="properties")
  private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
  @JsonProperty(value ="name")
  private final String name;
  @JsonProperty(value ="id")
  private final String id;
  @JsonProperty(value ="subjectName")
  private final String subjectName;
  @JsonProperty(value ="requiredFields")
  private List<ApiTypeArrayDTO> requiredFields = new ArrayList<ApiTypeArrayDTO>();

  private ApiSchemaDTO(String type, List<ApiTypeArrayDTO> properties, String name, String id, String subjectName, List<ApiTypeArrayDTO> requiredFields) {
    this.type = type;
    this.properties = properties;
    this.name = name;
    this.id = id;
    this.subjectName = subjectName;
    this.requiredFields = requiredFields;

    validateRequiredAttributes();
    validatePartialCombinations();
  }

  private ApiSchemaDTO(ApiSchemaDTOBuilder builder) {
    this.type = builder.type;
    this.properties = builder.properties;
    this.name = builder.name;
    this.id = builder.id;
    this.subjectName = builder.subjectName;
    this.requiredFields = builder.requiredFields;

    validateRequiredAttributes();
    validatePartialCombinations();
  }

  public static class ApiSchemaDTOBuilder {

    private String type;
    private List<ApiTypeArrayDTO> properties = new ArrayList<ApiTypeArrayDTO>();
    private String name;
    private String id;
    private String subjectName;
    private List<ApiTypeArrayDTO> requiredFields = new ArrayList<ApiTypeArrayDTO>();

    public ApiSchemaDTO.ApiSchemaDTOBuilder type(String type) {
      this.type = type;
      return this;
    }
    public ApiSchemaDTO.ApiSchemaDTOBuilder properties(List<ApiTypeArrayDTO> properties) {
      if (!properties.isEmpty()) {
        this.properties.addAll(properties);
      }
      return this;
    }

    public ApiSchemaDTO.ApiSchemaDTOBuilder propertie(ApiTypeArrayDTO propertie) {
      if (propertie != null) {
        this.properties.add(propertie);
      }
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
      if (requiredField != null) {
        this.requiredFields.add(requiredField);
      }
      return this;
    }

    public ApiSchemaDTO build() {
      ApiSchemaDTO apiSchemaDTO = new ApiSchemaDTO(this);
      return apiSchemaDTO;
    }
  }

  /**
  * Get type
  * @return type
  */
  @Schema(name = "type", required = true)
  public String getType() {
    return type;
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
  * Get name
  * @return name
  */
  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = true)
  public String getId() {
    return id;
  }

  /**
  * Get subjectName
  * @return subjectName
  */
  @Schema(name = "subjectName", required = true)
  public String getSubjectName() {
    return subjectName;
  }

  /**
  * Get requiredFields
  * @return requiredFields
  */
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
    return Objects.equals(this.type, apiSchemaDTO.type) && Objects.equals(this.properties, apiSchemaDTO.properties) && Objects.equals(this.name, apiSchemaDTO.name) && Objects.equals(this.id, apiSchemaDTO.id) && Objects.equals(this.subjectName, apiSchemaDTO.subjectName) && Objects.equals(this.requiredFields, apiSchemaDTO.requiredFields);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, properties, name, id, subjectName, requiredFields);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiSchemaDTO {\n");
    sb.append(" type: ").append(toIndentedString(type)).append("\n");
    sb.append(" properties: ").append(toIndentedString(properties)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" subjectName: ").append(toIndentedString(subjectName)).append("\n");
    sb.append(" requiredFields: ").append(toIndentedString(requiredFields)).append("\n");
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

    if (Objects.nonNull(this.type)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.properties)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.id)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.subjectName)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.requiredFields)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiSchemaDTO");
    }
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
