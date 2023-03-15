package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;
import com.sngular.multifileplugin.testCoconutSchema.model.exception.ModelClassException;

@Data
public class FieldDTO {

  @JsonProperty(value ="stringField")
  private StringFieldDTO stringField;

  @JsonProperty(value ="sequenceField")
  private SequenceFieldDTO sequenceField;

  @JsonProperty(value ="dateField")
  private DateFieldDTO dateField;

  @JsonProperty(value ="enumField")
  private EnumFieldDTO enumField;

  @JsonProperty(value ="booleanField")
  private BooleanFieldDTO booleanField;

  @JsonProperty(value ="arrayField")
  private ArrayFieldDTO arrayField;

  @JsonProperty(value ="numberField")
  private NumberFieldDTO numberField;

  @JsonProperty(value ="objectField")
  private ObjectFieldDTO objectField;

  @JsonProperty(value ="mapField")
  private MapFieldDTO mapField;

  @JsonProperty(value ="unionField")
  private UnionFieldDTO unionField;


  @Builder
  @Jacksonized
  private FieldDTO(StringFieldDTO stringField, SequenceFieldDTO sequenceField, DateFieldDTO dateField, EnumFieldDTO enumField, BooleanFieldDTO booleanField, ArrayFieldDTO arrayField, NumberFieldDTO numberField, ObjectFieldDTO objectField, MapFieldDTO mapField, UnionFieldDTO unionField) {
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
      throw new ModelClassException("FieldDTO");
    }
  }
}
