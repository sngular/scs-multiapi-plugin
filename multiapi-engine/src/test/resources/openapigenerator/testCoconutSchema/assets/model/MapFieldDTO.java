package com.sngular.multifileplugin.testCoconutSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class MapFieldDTO {

  @JsonProperty(value ="keyType")
  private String keyType;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="defaultValues")
  @Singular("defaultValue")
  private List<Object> defaultValues;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="mapSize")
  private Integer mapSize;

  @JsonProperty(value ="mapTypes")
  @Singular("mapType")
  private List<FieldDTO> mapTypes;


  @Builder
  @Jacksonized
  private MapFieldDTO(String keyType, String type, List<Object> defaultValues, String name, Integer mapSize, List<FieldDTO> mapTypes) {
    this.keyType = keyType;
    this.type = type;
    this.defaultValues = defaultValues;
    this.name = name;
    this.mapSize = mapSize;
    this.mapTypes = mapTypes;

  }

}
