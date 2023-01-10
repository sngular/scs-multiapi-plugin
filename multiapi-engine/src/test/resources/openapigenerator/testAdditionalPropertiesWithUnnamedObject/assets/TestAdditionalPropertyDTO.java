package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model.TestAdditionalPropertyAdditionalPropertyDTO;
import java.util.Map;
import java.util.HashMap;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

@Data
public class TestAdditionalPropertyDTO {

  @JsonProperty(value ="code")
  @NonNull
  private Integer code;

  @JsonProperty(value ="testAdditionalPropertyDTO")
  private Map<String, TestAdditionalPropertyAdditionalPropertyDTO> testAdditionalPropertyDTO = new HashMap<String, TestAdditionalPropertyAdditionalPropertyDTO>();

  @JsonProperty(value ="text")
  @NonNull
  private String text;


  @Builder
  private TestAdditionalPropertyDTO(@NonNull Integer code, Map<String, TestAdditionalPropertyAdditionalPropertyDTO> testAdditionalPropertyDTO, @NonNull String text) {
    this.code = code;
    this.testAdditionalPropertyDTO = testAdditionalPropertyDTO;
    this.text = text;

  }

}
