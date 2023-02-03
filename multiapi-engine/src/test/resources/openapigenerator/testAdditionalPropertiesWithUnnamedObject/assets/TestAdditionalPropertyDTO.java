package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model.TestAdditionalPropertyAdditionalPropertyDTO;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestAdditionalPropertyDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, TestAdditionalPropertyAdditionalPropertyDTO> additionalProperties = new HashMap<String, TestAdditionalPropertyAdditionalPropertyDTO>();

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

  @JsonProperty(value ="text")
  @NonNull
  private String text;


  @Builder
  @Jacksonized
  private TestAdditionalPropertyDTO(Map<String, TestAdditionalPropertyAdditionalPropertyDTO> additionalProperties, @NonNull Integer code, @NonNull String text) {
    this.additionalProperties = additionalProperties;
    this.code = code;
    this.text = text;

  }

}
