package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class TestAdditionalPropertyAdditionalPropertyDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="testAdditionalPropertyAdditionalPropertyDTO")
  private Map<String, Object> testAdditionalPropertyAdditionalPropertyDTO = new HashMap<String, Object>();


  @Builder
  private TestAdditionalPropertyAdditionalPropertyDTO(@NonNull String name, Map<String, Object> testAdditionalPropertyAdditionalPropertyDTO) {
    this.name = name;
    this.testAdditionalPropertyAdditionalPropertyDTO = testAdditionalPropertyAdditionalPropertyDTO;

  }

}
