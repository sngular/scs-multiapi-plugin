package net.coru.multifileplugin.testoneofInresponse.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;


@Data
public class GameInfoDTO {

  @JsonProperty(value ="rooms")
  @NonNull
  private Integer rooms;

  @JsonProperty(value ="gameName")
  @NonNull
  private String gameName;

  @JsonProperty(value ="players")
  @NonNull
  private List<String> players = new ArrayList<String>();


  @Builder
  private GameInfoDTO(@NonNull Integer rooms, @NonNull String gameName, @NonNull List<String> players){
    this.rooms = rooms;
    this.gameName = gameName;
    this.players = players;

  }

}
