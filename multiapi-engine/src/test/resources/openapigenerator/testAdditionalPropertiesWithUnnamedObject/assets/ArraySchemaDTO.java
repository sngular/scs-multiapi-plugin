package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ArraySchemaDTO {

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="index")
  private Integer index;


  @Builder
  @Jacksonized
  private ArraySchemaDTO(String name, Integer index) {
    this.name = name;
    this.index = index;

  }

}
