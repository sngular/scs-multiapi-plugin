package com.sngular.multifileplugin.testoneofinresponse.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;
import com.sngular.multifileplugin.testoneofinresponse.model.exception.ModelClassException;

@Data
public class InlineResponse200ListGamesOneOfDTO {

  @JsonProperty(value ="gameInfo")
  private GameInfoDTO gameInfo;

  @JsonProperty(value ="game")
  private GameDTO game;


  @Builder
  @Jacksonized
  private InlineResponse200ListGamesOneOfDTO(GameInfoDTO gameInfo, GameDTO game) {
    this.gameInfo = gameInfo;
    this.game = game;

    validatePartialCombinations();
  }

  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

    if (Objects.nonNull(this.gameInfo)) {
      satisfiedCondition = true;
    } else if (Objects.nonNull(this.game)) {
      satisfiedCondition = true;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("InlineResponse200ListGamesOneOfDTO");
    }
  }
}
