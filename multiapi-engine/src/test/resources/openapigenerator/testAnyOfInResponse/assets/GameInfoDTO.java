package com.sngular.multifileplugin.testanyofinresponse.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Value;
import lombok.NonNull;
import lombok.Singular;
import lombok.extern.jackson.Jacksonized;

@Value
public class GameInfoDTO {

  @JsonProperty(value ="gameName")
  @NonNull
  private String gameName;

  @JsonProperty(value ="rooms")
  @NonNull
  private Integer rooms;

  @JsonProperty(value ="players")
  @Singular("player")
  private List<String> players;


  @Builder
  @Jacksonized
  private GameInfoDTO(@NonNull String gameName, @NonNull Integer rooms, List<String> players) {
    this.gameName = gameName;
    this.rooms = rooms;
    this.players = players;

  }

}
