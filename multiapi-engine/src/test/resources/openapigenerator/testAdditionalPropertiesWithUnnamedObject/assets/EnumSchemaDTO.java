package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
public class EnumSchemaDTO {

  @JsonProperty(value ="enumSchema")
  private String enumSchema;


  @Builder
  private EnumSchemaDTO(String enumSchema) {
    this.enumSchema = enumSchema;

  }

}
