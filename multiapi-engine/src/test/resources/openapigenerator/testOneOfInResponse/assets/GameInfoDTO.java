package net.coru.multifileplugin.testoneofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

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
  private GameInfoDTO(@NonNull String gameName, @NonNull Integer rooms, List<String> players) {
    this.gameName = gameName;
    this.rooms = rooms;
    this.players = players;

  }

}
