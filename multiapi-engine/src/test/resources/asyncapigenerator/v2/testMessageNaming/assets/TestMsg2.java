package com.sngular.scsplugin.messagenaming.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class TestMsg2 {

  @JsonProperty(value ="name")
  private String name;

  @JsonProperty(value ="id")
  private Integer id;


  @Builder
  @Jacksonized
  private TestMsg2(String name, Integer id) {
    this.name = name;
    this.id = id;

  }

}
