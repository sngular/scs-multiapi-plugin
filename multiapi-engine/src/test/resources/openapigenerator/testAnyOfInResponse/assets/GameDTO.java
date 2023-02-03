package com.sngular.multifileplugin.testanyofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
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
