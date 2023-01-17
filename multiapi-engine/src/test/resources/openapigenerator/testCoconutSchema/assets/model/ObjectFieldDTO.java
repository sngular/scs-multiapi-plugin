package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
public class ObjectFieldDTO {

  @JsonProperty(value ="requiredValues")
  private List<String> requiredValues = new ArrayList<String>();

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="fieldDTO")
  private List<FieldDTO> fieldDTO = new ArrayList<FieldDTO>();


  @Builder
  private ObjectFieldDTO(List<String> requiredValues, String type, List<Object> defaultValues, String name, List<FieldDTO> fieldDTO) {
    this.requiredValues = requiredValues;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.fieldDTO = fieldDTO;

  }

}
