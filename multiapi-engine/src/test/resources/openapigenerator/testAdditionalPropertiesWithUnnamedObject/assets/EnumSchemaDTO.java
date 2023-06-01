package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class EnumSchemaDTO {

  @JsonProperty(value ="enumSchema")
  private EnumSchema enumSchema;

  public enum EnumSchema {
    ASC("asc"),
    DESC("desc");

    private String value;

    EnumSchema(String value) {
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

  @Builder
  @Jacksonized
  private EnumSchemaDTO(EnumSchema enumSchema) {
    this.enumSchema = enumSchema;

  }

}
