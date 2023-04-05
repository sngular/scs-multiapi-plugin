package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

@JsonDeserialize(builder = ApiTypeArrayDTO.ApiTypeArrayDTOBuilder.class)
public class ApiTypeArrayDTO {

  @JsonProperty(value ="stringField")
  private ApiStringFieldDTO stringField;
  @JsonProperty(value ="sequenceField")
  private ApiSequenceFieldDTO sequenceField;
  @JsonProperty(value ="dateField")
  private ApiDateFieldDTO dateField;
  @JsonProperty(value ="enumField")
  private ApiEnumFieldDTO enumField;
  @JsonProperty(value ="booleanField")
  private ApiBooleanFieldDTO booleanField;
  @JsonProperty(value ="arrayField")
  private ApiArrayFieldDTO arrayField;
  @JsonProperty(value ="numberField")
  private ApiNumberFieldDTO numberField;
  @JsonProperty(value ="objectField")
  private ApiObjectFieldDTO objectField;
  @JsonProperty(value ="mapField")
  private ApiMapFieldDTO mapField;
  @JsonProperty(value ="unionField")
  private ApiUnionFieldDTO unionField;

  private ApiTypeArrayDTO(ApiStringFieldDTO stringField, ApiSequenceFieldDTO sequenceField, ApiDateFieldDTO dateField, ApiEnumFieldDTO enumField, ApiBooleanFieldDTO booleanField, ApiArrayFieldDTO arrayField, ApiNumberFieldDTO numberField, ApiObjectFieldDTO objectField, ApiMapFieldDTO mapField, ApiUnionFieldDTO unionField) {
    this.stringField = stringField;
    this.sequenceField = sequenceField;
    this.dateField = dateField;
    this.enumField = enumField;
    this.booleanField = booleanField;
    this.arrayField = arrayField;
    this.numberField = numberField;
    this.objectField = objectField;
    this.mapField = mapField;
    this.unionField = unionField;

    validatePartialCombinations();
  }

  private ApiTypeArrayDTO(ApiTypeArrayDTOBuilder builder) {
    this.stringField = builder.stringField;
    this.sequenceField = builder.sequenceField;
    this.dateField = builder.dateField;
    this.enumField = builder.enumField;
    this.booleanField = builder.booleanField;
    this.arrayField = builder.arrayField;
    this.numberField = builder.numberField;
    this.objectField = builder.objectField;
    this.mapField = builder.mapField;
    this.unionField = builder.unionField;

    validatePartialCombinations();
  }

  public static ApiTypeArrayDTO.ApiTypeArrayDTOBuilder builder() {
    return new ApiTypeArrayDTO.ApiTypeArrayDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiTypeArrayDTOBuilder {

    private ApiStringFieldDTO stringField;
    private ApiSequenceFieldDTO sequenceField;
    private ApiDateFieldDTO dateField;
    private ApiEnumFieldDTO enumField;
    private ApiBooleanFieldDTO booleanField;
    private ApiArrayFieldDTO arrayField;
    private ApiNumberFieldDTO numberField;
    private ApiObjectFieldDTO objectField;
    private ApiMapFieldDTO mapField;
    private ApiUnionFieldDTO unionField;
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder stringField(ApiStringFieldDTO stringField) {
      this.stringField = stringField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder sequenceField(ApiSequenceFieldDTO sequenceField) {
      this.sequenceField = sequenceField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder dateField(ApiDateFieldDTO dateField) {
      this.dateField = dateField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder enumField(ApiEnumFieldDTO enumField) {
      this.enumField = enumField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder booleanField(ApiBooleanFieldDTO booleanField) {
      this.booleanField = booleanField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder arrayField(ApiArrayFieldDTO arrayField) {
      this.arrayField = arrayField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder numberField(ApiNumberFieldDTO numberField) {
      this.numberField = numberField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder objectField(ApiObjectFieldDTO objectField) {
      this.objectField = objectField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder mapField(ApiMapFieldDTO mapField) {
      this.mapField = mapField;
      return this;
    }
    public ApiTypeArrayDTO.ApiTypeArrayDTOBuilder unionField(ApiUnionFieldDTO unionField) {
      this.unionField = unionField;
      return this;
    }

    public ApiTypeArrayDTO build() {
      ApiTypeArrayDTO apiTypeArrayDTO = new ApiTypeArrayDTO(this);
      return apiTypeArrayDTO;
    }
  }

  /**
  * Get stringField
  * @return stringField
  */
  @Schema(name = "stringField", required = false)
  public ApiStringFieldDTO getStringField() {
    return stringField;
  }
  public void setStringField(ApiStringFieldDTO stringField) {
    this.stringField = stringField;
  }

  /**
  * Get sequenceField
  * @return sequenceField
  */
  @Schema(name = "sequenceField", required = false)
  public ApiSequenceFieldDTO getSequenceField() {
    return sequenceField;
  }
  public void setSequenceField(ApiSequenceFieldDTO sequenceField) {
    this.sequenceField = sequenceField;
  }

  /**
  * Get dateField
  * @return dateField
  */
  @Schema(name = "dateField", required = false)
  public ApiDateFieldDTO getDateField() {
    return dateField;
  }
  public void setDateField(ApiDateFieldDTO dateField) {
    this.dateField = dateField;
  }

  /**
  * Get enumField
  * @return enumField
  */
  @Schema(name = "enumField", required = false)
  public ApiEnumFieldDTO getEnumField() {
    return enumField;
  }
  public void setEnumField(ApiEnumFieldDTO enumField) {
    this.enumField = enumField;
  }

  /**
  * Get booleanField
  * @return booleanField
  */
  @Schema(name = "booleanField", required = false)
  public ApiBooleanFieldDTO getBooleanField() {
    return booleanField;
  }
  public void setBooleanField(ApiBooleanFieldDTO booleanField) {
    this.booleanField = booleanField;
  }

  /**
  * Get arrayField
  * @return arrayField
  */
  @Schema(name = "arrayField", required = false)
  public ApiArrayFieldDTO getArrayField() {
    return arrayField;
  }
  public void setArrayField(ApiArrayFieldDTO arrayField) {
    this.arrayField = arrayField;
  }

  /**
  * Get numberField
  * @return numberField
  */
  @Schema(name = "numberField", required = false)
  public ApiNumberFieldDTO getNumberField() {
    return numberField;
  }
  public void setNumberField(ApiNumberFieldDTO numberField) {
    this.numberField = numberField;
  }

  /**
  * Get objectField
  * @return objectField
  */
  @Schema(name = "objectField", required = false)
  public ApiObjectFieldDTO getObjectField() {
    return objectField;
  }
  public void setObjectField(ApiObjectFieldDTO objectField) {
    this.objectField = objectField;
  }

  /**
  * Get mapField
  * @return mapField
  */
  @Schema(name = "mapField", required = false)
  public ApiMapFieldDTO getMapField() {
    return mapField;
  }
  public void setMapField(ApiMapFieldDTO mapField) {
    this.mapField = mapField;
  }

  /**
  * Get unionField
  * @return unionField
  */
  @Schema(name = "unionField", required = false)
  public ApiUnionFieldDTO getUnionField() {
    return unionField;
  }
  public void setUnionField(ApiUnionFieldDTO unionField) {
    this.unionField = unionField;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTypeArrayDTO apiTypeArrayDTO = (ApiTypeArrayDTO) o;
    return Objects.equals(this.stringField, apiTypeArrayDTO.stringField) && Objects.equals(this.sequenceField, apiTypeArrayDTO.sequenceField) && Objects.equals(this.dateField, apiTypeArrayDTO.dateField) && Objects.equals(this.enumField, apiTypeArrayDTO.enumField) && Objects.equals(this.booleanField, apiTypeArrayDTO.booleanField) && Objects.equals(this.arrayField, apiTypeArrayDTO.arrayField) && Objects.equals(this.numberField, apiTypeArrayDTO.numberField) && Objects.equals(this.objectField, apiTypeArrayDTO.objectField) && Objects.equals(this.mapField, apiTypeArrayDTO.mapField) && Objects.equals(this.unionField, apiTypeArrayDTO.unionField);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringField, sequenceField, dateField, enumField, booleanField, arrayField, numberField, objectField, mapField, unionField);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTypeArrayDTO {\n");
    sb.append(" stringField: ").append(toIndentedString(stringField)).append("\n");
    sb.append(" sequenceField: ").append(toIndentedString(sequenceField)).append("\n");
    sb.append(" dateField: ").append(toIndentedString(dateField)).append("\n");
    sb.append(" enumField: ").append(toIndentedString(enumField)).append("\n");
    sb.append(" booleanField: ").append(toIndentedString(booleanField)).append("\n");
    sb.append(" arrayField: ").append(toIndentedString(arrayField)).append("\n");
    sb.append(" numberField: ").append(toIndentedString(numberField)).append("\n");
    sb.append(" objectField: ").append(toIndentedString(objectField)).append("\n");
    sb.append(" mapField: ").append(toIndentedString(mapField)).append("\n");
    sb.append(" unionField: ").append(toIndentedString(unionField)).append("\n");
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

    if (Objects.nonNull(this.stringField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.sequenceField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.dateField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.enumField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.booleanField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.arrayField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.numberField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.objectField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.mapField)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.unionField)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTypeArrayDTO");
    }
  }


}
