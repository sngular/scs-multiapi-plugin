package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class EnumSchemaDTO {

  @JsonProperty(value ="enumSchema")
  private String enumSchema;


  @Builder
  @Jacksonized
  private EnumSchemaDTO(String enumSchema) {
    this.enumSchema = enumSchema;

  }

}
