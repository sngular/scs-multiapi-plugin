package com.sngular.scsplugin.arraywithstring.model.event;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import lombok.Builder;
import lombok.Singular;
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

@Value
public class ObjectArrayDTO {

  @JsonProperty(value ="idObject")
  private Integer idObject;

  @JsonProperty(value ="info")
  private String info;

  @JsonProperty(value ="players")
  @Singular("player")
  private List<String> players;


  @Builder
  @Jacksonized
  private ObjectArrayDTO(Integer idObject, String info, List<String> players) {
    this.idObject = idObject;
    this.info = info;
    this.players = players;

  }

}
