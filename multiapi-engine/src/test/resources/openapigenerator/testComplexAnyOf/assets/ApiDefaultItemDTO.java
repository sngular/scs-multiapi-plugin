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
  private StringField stringField;
  @JsonProperty(value ="sequenceField")
  private ApiSequenceFieldDTO sequenceField;
  @JsonProperty(value ="dateField")
  private DateField dateField;
  @JsonProperty(value ="enumField")
  private EnumField enumField;
  @JsonProperty(value ="booleanField")
  private BooleanField booleanField;
  @JsonProperty(value ="arrayField")
  private ArrayField arrayField;
  @JsonProperty(value ="numberField")
  private NumberField numberField;
  @JsonProperty(value ="objectField")
  private ObjectField objectField;
  @JsonProperty(value ="mapField")
  private MapField mapField;
  @JsonProperty(value ="unionField")
  private ApiUnionFieldDTO unionField;

  private ApiDefaultItemDTO(StringField stringField, ApiSequenceFieldDTO sequenceField, DateField dateField, EnumField enumField, BooleanField booleanField, ArrayField arrayField, NumberField numberField, ObjectField objectField, MapField mapField, ApiUnionFieldDTO unionField) {
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

    private StringField stringField;
    private ApiSequenceFieldDTO sequenceField;
    private DateField dateField;
    private EnumField enumField;
    private BooleanField booleanField;
    private ArrayField arrayField;
    private NumberField numberField;
    private ObjectField objectField;
    private MapField mapField;
    private ApiUnionFieldDTO unionField;

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder stringField(StringField stringField) {
      this.stringField = stringField;
      return this;
    }
    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder sequenceField(ApiSequenceFieldDTO sequenceField) {
      this.sequenceField = sequenceField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder dateField(DateField dateField) {
      this.dateField = dateField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder enumField(EnumField enumField) {
      this.enumField = enumField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder booleanField(BooleanField booleanField) {
      this.booleanField = booleanField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder arrayField(ArrayField arrayField) {
      this.arrayField = arrayField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder numberField(NumberField numberField) {
      this.numberField = numberField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder objectField(ObjectField objectField) {
      this.objectField = objectField;
      return this;
    }

    public ApiDefaultItemDTO.ApiDefaultItemDTOBuilder mapField(MapField mapField) {
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
  public StringField getStringField() {
    return stringField;
  }
  public void setStringField(StringField stringField) {
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
  public DateField getDateField() {
    return dateField;
  }
  public void setDateField(DateField dateField) {
    this.dateField = dateField;
  }


  @Schema(name = "enumField", required = false)
  public EnumField getEnumField() {
    return enumField;
  }
  public void setEnumField(EnumField enumField) {
    this.enumField = enumField;
  }


  @Schema(name = "booleanField", required = false)
  public BooleanField getBooleanField() {
    return booleanField;
  }
  public void setBooleanField(BooleanField booleanField) {
    this.booleanField = booleanField;
  }


  @Schema(name = "arrayField", required = false)
  public ArrayField getArrayField() {
    return arrayField;
  }
  public void setArrayField(ArrayField arrayField) {
    this.arrayField = arrayField;
  }


  @Schema(name = "numberField", required = false)
  public NumberField getNumberField() {
    return numberField;
  }
  public void setNumberField(NumberField numberField) {
    this.numberField = numberField;
  }


  @Schema(name = "objectField", required = false)
  public ObjectField getObjectField() {
    return objectField;
  }
  public void setObjectField(ObjectField objectField) {
    this.objectField = objectField;
  }


  @Schema(name = "mapField", required = false)
  public MapField getMapField() {
    return mapField;
  }
  public void setMapField(MapField mapField) {
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