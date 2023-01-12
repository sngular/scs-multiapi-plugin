package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
public class ArraySchemaDTO {

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="index")
  private Integer index;


  @Builder
  private ArraySchemaDTO(String name, Integer index) {
    this.name = name;
    this.index = index;

  }

}