package com.sngular.multifileplugin.testissuefaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Value;
import lombok.Singular;
import lombok.extern.jackson.Jacksonized;

@Value
public class ConfigurationDTO {

  @JsonProperty(value ="configuration")
  @Singular("_configuration")
  private Map<String, BigDecimal> configuration;

  @JsonProperty(value ="numberToGenerate")
  private Integer numberToGenerate;

  @JsonProperty(value ="schema")
  private SchemaDTO schema;


  @Builder
  @Jacksonized
  private ConfigurationDTO(Map<String, BigDecimal> configuration, Integer numberToGenerate, SchemaDTO schema) {
    this.configuration = configuration;
    this.numberToGenerate = numberToGenerate;
    this.schema = schema;

  }

}
