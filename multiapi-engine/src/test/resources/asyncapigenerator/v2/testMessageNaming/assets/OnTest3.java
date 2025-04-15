package com.sngular.scsplugin.messagenaming.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class OnTest3 {

  @JsonProperty(value ="id")
  private Integer id;

  @JsonProperty(value ="age")
  private Integer age;


  @Builder
  @Jacksonized
  private OnTest3(Integer id, Integer age) {
    this.id = id;
    this.age = age;

  }

}
