package com.sngular.multifileplugin.testanyofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import com.sngular.multifileplugin.testanyofinresponse.model.exception.ModelClassException;

@Data
public class InlineResponse200ListGamesAnyOfDTO {

  @JsonProperty(value ="gameName")
  @NonNull
  private String gameName;

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;

  @JsonProperty(value ="rooms")
  @NonNull
  private Integer rooms;

  @JsonProperty(value ="players")
  private List<String> players = new ArrayList<String>();


  @Builder
  @JsonPOJOBuilder
  private InlineResponse200ListGamesAnyOfDTO(@NonNull String gameName, @NonNull String name, @NonNull Integer id, @NonNull Integer rooms, List<String> players) {
    this.gameName = gameName;
    this.name = name;
    this.id = id;
    this.rooms = rooms;
    this.players = players;

    validatePartialCombinations();
  }

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.gameName)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.id)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.rooms)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.players)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("InlineResponse200ListGamesAnyOfDTO");
    }
  }
}
