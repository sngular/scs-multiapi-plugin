package com.sngular.multifileplugin.testIssueFaker.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class ConfigurationDTO {

  @JsonProperty(value ="configuration")
  private Map<String, BigDecimal> configuration = new HashMap<String, BigDecimal>();

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
