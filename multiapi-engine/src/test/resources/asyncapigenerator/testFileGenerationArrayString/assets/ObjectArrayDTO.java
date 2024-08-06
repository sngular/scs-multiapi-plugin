package com.sngular.scsplugin.arraywithstring.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = ObjectArrayDTO.ObjectArrayDTOBuilder.class)
public class ObjectArrayDTO {

  @JsonProperty(value ="idObject")
  private Integer idObject;
  @JsonProperty(value ="info")
  private String info;
  @JsonProperty(value ="players")
  private List<String> players = new ArrayList<String>();

  private ObjectArrayDTO(ObjectArrayDTOBuilder builder) {
    this.idObject = builder.idObject;
    this.info = builder.info;
    this.players = builder.players;

  }

  public static ObjectArrayDTO.ObjectArrayDTOBuilder builder() {
    return new ObjectArrayDTO.ObjectArrayDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ObjectArrayDTOBuilder {

    private Integer idObject;
    private String info;
    private List<String> players = new ArrayList<String>();

    public ObjectArrayDTO.ObjectArrayDTOBuilder idObject(Integer idObject) {
      this.idObject = idObject;
      return this;
    }

    public ObjectArrayDTO.ObjectArrayDTOBuilder info(String info) {
      this.info = info;
      return this;
    }
    public ObjectArrayDTO.ObjectArrayDTOBuilder players(List<String> players) {
      if (!players.isEmpty()) {
        this.players.addAll(players);
      }
      return this;
    }

    public ObjectArrayDTO.ObjectArrayDTOBuilder player(String player) {
      if (player != null) {
        this.players.add(player);
      }
      return this;
    }

    public ObjectArrayDTO build() {
      ObjectArrayDTO objectArrayDTO = new ObjectArrayDTO(this);
      return objectArrayDTO;
    }
  }

  @Schema(name = "idObject", required = false)
  public Integer getIdObject() {
    return idObject;
  }
  public void setIdObject(Integer idObject) {
    this.idObject = idObject;
  }

  @Schema(name = "info", required = false)
  public String getInfo() {
    return info;
  }
  public void setInfo(String info) {
    this.info = info;
  }

  @Schema(name = "players", required = false)
  public List<String> getPlayers() {
    return players;
  }
  public void setPlayers(List<String> players) {
    this.players = players;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ObjectArrayDTO objectArrayDTO = (ObjectArrayDTO) o;
    return Objects.equals(this.idObject, objectArrayDTO.idObject) && Objects.equals(this.info, objectArrayDTO.info) && Objects.equals(this.players, objectArrayDTO.players);
  }

  @Override
  public int hashCode() {
    return Objects.hash(idObject, info, players);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ObjectArrayDTO{");
    sb.append(" idObject:").append(idObject).append(",");
    sb.append(" info:").append(info).append(",");
    sb.append(" players:").append(players);
    sb.append("}");
    return sb.toString();
  }


}
