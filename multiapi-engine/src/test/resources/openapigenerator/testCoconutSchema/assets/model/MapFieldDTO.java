package com.sngular.multifileplugin.testCoconutSchema.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;

@Data
public class MapFieldDTO {

  @JsonProperty(value ="keyType")
  private String keyType;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  private List<Object> defaultValues = new ArrayList<Object>();

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="mapSize")
  private Integer mapSize;

  @JsonProperty(value ="fieldDTO")
  private List<FieldDTO> fieldDTO = new ArrayList<FieldDTO>();


  @Builder
  @JsonPOJOBuilder
  private MapFieldDTO(String keyType, String type, List<Object> defaultValues, String name, Integer mapSize, List<FieldDTO> fieldDTO) {
    this.keyType = keyType;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.mapSize = mapSize;
    this.fieldDTO = fieldDTO;

  }

}
