package com.sngular.multifileplugin.testcomplexanyof.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.testcomplexanyof.model.exception.ModelClassException;

@JsonDeserialize(builder = ApiDefaultItemDTO.ApiDefaultItemDTOBuilder.class)
public class ApiDefaultItemDTO {

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

  private ApiDefaultItemDTO(ApiStringFieldDTO stringField, ApiSequenceFieldDTO sequenceField, ApiDateFieldDTO dateField, ApiEnumFieldDTO enumField, ApiBooleanFieldDTO booleanField, ApiArrayFieldDTO arrayField, ApiNumberFieldDTO numberField, ApiObjectFieldDTO objectField, ApiMapFieldDTO mapField, ApiUnionFieldDTO unionField) {
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

  private ApiDefaultItemDTO(ApiDefaultItemDTOBuilder builder) {
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

  public static ApiDefaultItemDTO.ApiDefaultItemDTOBuilder builder() {
    return new ApiDefaultItemDTO.ApiDefaultItemDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiDefaultItemDTOBuilder {

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

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder stringField(ApiStringFieldDTO stringField) {
      this.stringField = stringField;
      return this;
    }
    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder sequenceField(ApiSequenceFieldDTO sequenceField) {
      this.sequenceField = sequenceField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder dateField(ApiDateFieldDTO dateField) {
      this.dateField = dateField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder enumField(ApiEnumFieldDTO enumField) {
      this.enumField = enumField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder booleanField(ApiBooleanFieldDTO booleanField) {
      this.booleanField = booleanField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder arrayField(ApiArrayFieldDTO arrayField) {
      this.arrayField = arrayField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder numberField(ApiNumberFieldDTO numberField) {
      this.numberField = numberField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder objectField(ApiObjectFieldDTO objectField) {
      this.objectField = objectField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder mapField(ApiMapFieldDTO mapField) {
      this.mapField = mapField;
      return this;
    }
    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder unionField(ApiUnionFieldDTO unionField) {
      this.unionField = unionField;
      return this;
    }

    public ApiDefaultItemDTO build() {
      ApiDefaultItemDTO apiDefaultItemDTO = new ApiDefaultItemDTO(this);
      return apiDefaultItemDTO;
    }
  }


  @Schema(name = "stringField", required = false)
  public ApiStringFieldDTO getApiStringFieldDTO() {
    return stringField;
  }
  public void setApiStringFieldDTO(ApiStringFieldDTO stringField) {
    this.stringField = stringField;
  }


  @Schema(name = "sequenceField", required = false)
  public ApiSequenceFieldDTO getSequenceField() {
    return sequenceField;
  }
  public void setSequenceField(ApiSequenceFieldDTO sequenceField) {
    this.sequenceField = sequenceField;
  }


  @Schema(name = "dateField", required = false)
  public ApiDateFieldDTO getApiDateFieldDTO() {
    return dateField;
  }
  public void setApiDateFieldDTO(ApiDateFieldDTO dateField) {
    this.dateField = dateField;
  }


  @Schema(name = "enumField", required = false)
  public ApiEnumFieldDTO getApiEnumFieldDTO() {
    return enumField;
  }
  public void setApiEnumFieldDTO(ApiEnumFieldDTO enumField) {
    this.enumField = enumField;
  }


  @Schema(name = "booleanField", required = false)
  public ApiBooleanFieldDTO getApiBooleanFieldDTO() {
    return booleanField;
  }
  public void setApiBooleanFieldDTO(ApiBooleanFieldDTO booleanField) {
    this.booleanField = booleanField;
  }


  @Schema(name = "arrayField", required = false)
  public ApiArrayFieldDTO getApiArrayFieldDTO() {
    return arrayField;
  }
  public void setApiArrayFieldDTO(ApiArrayFieldDTO arrayField) {
    this.arrayField = arrayField;
  }


  @Schema(name = "numberField", required = false)
  public ApiNumberFieldDTO getApiNumberFieldDTO() {
    return numberField;
  }
  public void setApiNumberFieldDTO(ApiNumberFieldDTO numberField) {
    this.numberField = numberField;
  }


  @Schema(name = "objectField", required = false)
  public ApiObjectFieldDTO getApiObjectFieldDTO() {
    return objectField;
  }
  public void setApiObjectFieldDTO(ApiObjectFieldDTO objectField) {
    this.objectField = objectField;
  }


  @Schema(name = "mapField", required = false)
  public ApiMapFieldDTO getApiMapFieldDTO() {
    return mapField;
  }
  public void setApiMapFieldDTO(ApiMapFieldDTO mapField) {
    this.mapField = mapField;
  }


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
    ApiDefaultItemDTO apiDefaultItemDTO = (ApiDefaultItemDTO) o;
    return Objects.equals(this.stringField, apiDefaultItemDTO.stringField) && Objects.equals(this.sequenceField, apiDefaultItemDTO.sequenceField) && Objects.equals(this.dateField, apiDefaultItemDTO.dateField) && Objects.equals(this.enumField, apiDefaultItemDTO.enumField) && Objects.equals(this.booleanField, apiDefaultItemDTO.booleanField) && Objects.equals(this.arrayField, apiDefaultItemDTO.arrayField) && Objects.equals(this.numberField, apiDefaultItemDTO.numberField) && Objects.equals(this.objectField, apiDefaultItemDTO.objectField) && Objects.equals(this.mapField, apiDefaultItemDTO.mapField) && Objects.equals(this.unionField, apiDefaultItemDTO.unionField);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringField, sequenceField, dateField, enumField, booleanField, arrayField, numberField, objectField, mapField, unionField);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiDefaultItemDTO {\n");
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
      throw new ModelClassException("ApiDefaultItemDTO");
    }
  }

}