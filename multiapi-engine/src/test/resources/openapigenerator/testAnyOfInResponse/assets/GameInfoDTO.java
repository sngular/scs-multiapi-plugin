package com.sngular.multifileplugin.testanyofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class GameInfoDTO {

  @JsonProperty(value ="gameName")
  @NonNull
  private String gameName;

  @JsonProperty(value ="rooms")
  @NonNull
  private Integer rooms;

  @JsonProperty(value ="players")
  private List<String> players = new ArrayList<String>();


  @Builder
  @Jacksonized
  private GameInfoDTO(@NonNull String gameName, @NonNull Integer rooms, List<String> players) {
    this.gameName = gameName;
    this.rooms = rooms;
    this.players = players;

  }

}
