package com.sngular.multifileplugin.testoneofinresponse.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class GameDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;


  @Builder
  @Jacksonized
  private GameDTO(@NonNull String name, @NonNull Integer id) {
    this.name = name;
    this.id = id;

  }

}
