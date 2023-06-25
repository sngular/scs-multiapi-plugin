package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class TestValueDTO {

  @JsonProperty(value ="additionalProperties")
  private Map<String, TestValueValueDTO> additionalProperties = new HashMap<String, TestValueValueDTO>();

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

  @JsonProperty(value ="text")
  @NonNull
  private String text;


  @Builder
  @Jacksonized
  private TestValueDTO(Map<String, TestValueValueDTO> additionalProperties, @NonNull Integer code, @NonNull String text) {
    this.additionalProperties = additionalProperties;
    this.code = code;
    this.text = text;

  }

}
