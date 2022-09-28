package net.coru.multifileplugin.testoneofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import net.coru.multifileplugin.testoneofinresponse.model.exception.ModelClassException;

@Data
public class InlineResponse200ListGamesOneOfDTO {

  @JsonProperty(value ="name")
  @NonNull
  private String name;

  @JsonProperty(value ="id")
  @NonNull
  private Integer id;

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
  private InlineResponse200ListGamesOneOfDTO(@NonNull String name, @NonNull Integer id, @NonNull Integer rooms, @NonNull String gameName, @NonNull List<String> players) {
    this.name = name;
    this.id = id;
    this.rooms = rooms;
    this.gameName = gameName;
    this.players = players;

    validatePartialCombinations();
  }

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.name)) {
      satisfiedCondition = true;
    }
    else if (Objects.nonNull(this.id)) {
      satisfiedCondition = true;
    }
    else if (Objects.nonNull(this.rooms)) {
      satisfiedCondition = true;
    }
    else if (Objects.nonNull(this.gameName)) {
      satisfiedCondition = true;
    }
    else if (Objects.nonNull(this.players)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("InlineResponse200ListGamesOneOfDTO");
    }
  }
}
