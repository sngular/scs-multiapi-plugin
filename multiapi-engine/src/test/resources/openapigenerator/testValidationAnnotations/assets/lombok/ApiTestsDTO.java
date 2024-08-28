package com.sngular.multifileplugin.lombok.testapi.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;

@Value
public class ApiTestsDTO {

  @JsonProperty(value ="apiTestDTO")
  private ApiTestDTO apiTestDTO;


  @Builder
  private ApiTestsDTO(ApiTestDTO apiTestDTO) {
    this.apiTestDTO = apiTestDTO;

  }

}
