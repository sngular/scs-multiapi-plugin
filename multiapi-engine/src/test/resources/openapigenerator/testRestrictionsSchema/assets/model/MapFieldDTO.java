package com.sngular.multifileplugin.testRestrictionsSchema.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class MapFieldDTO {

  @JsonProperty(value ="mandatory")
  private Boolean mandatory;

  @JsonProperty(value ="keyType")
  private String keyType;

  @JsonProperty(value ="type")
  private String type;

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="mapSize")
  private Integer mapSize;

  @JsonProperty(value ="mapTypes")
  @Singular("mapType")
  private List<FieldDTO> mapTypes;

  @JsonProperty(value ="defaultValue")
  @Singular("_defaultValue")
  private List<Object> defaultValue;


  @Builder
  @Jacksonized
  private MapFieldDTO(Boolean mandatory, String keyType, String type, String name, Integer mapSize, List<FieldDTO> mapTypes, List<Object> defaultValue) {
    this.mandatory = mandatory;
    this.keyType = keyType;
    this.type = type;
    this.name = name;
    this.mapSize = mapSize;
    this.mapTypes = mapTypes;
    this.defaultValue = defaultValue;

  }

}
