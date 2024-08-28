package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class FieldDTO {

  @JsonProperty(value ="typeField")
  private TypeField typeField;

  public enum TypeField {
    MAPFIELD("MapField"),
    STRINGFIELD("StringField"),
    ARRAYFIELD("ArrayField"),
    BOOLEANFIELD("BooleanField"),
    UUIDFIELD("UUIDField"),
    ENUMFIELD("EnumField"),
    NUMBERFIELD("NumberField"),
    OBJECTFIELD("ObjectField"),
    DATEFIELD("DateField"),
    SEQUENCEFIELD("SequenceField"),
    UNIONFIELD("UnionField");

    private String value;

    TypeField(String value) {
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

  @JsonProperty(value ="fieldValue")
  private FieldValueDTO fieldValue;


  @Builder
  @Jacksonized
  private FieldDTO(TypeField typeField, FieldValueDTO fieldValue) {
    this.typeField = typeField;
    this.fieldValue = fieldValue;

  }

}
