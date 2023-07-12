package com.sngular.scsplugin.arraywithstring.model.event.schemas;

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
  @JsonProperty(value ="players")
  private List<String> players = new ArrayList<String>();
  @JsonProperty(value ="info")
  private String info;

  private ObjectArrayDTO(Integer idObject, List<String> players, String info) {
    this.idObject = idObject;
    this.players = players;
    this.info = info;

  }

  private ObjectArrayDTO(ObjectArrayDTOBuilder builder) {
    this.idObject = builder.idObject;
    this.players = builder.players;
    this.info = builder.info;

  }

  public static ObjectArrayDTO.ObjectArrayDTOBuilder builder() {
    return new ObjectArrayDTO.ObjectArrayDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ObjectArrayDTOBuilder {

    private Integer idObject;
    private List<String> players = new ArrayList<String>();
    private String info;

    public ObjectArrayDTO.ObjectArrayDTOBuilder idObject(Integer idObject) {
      this.idObject = idObject;
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

    public ObjectArrayDTO.ObjectArrayDTOBuilder info(String info) {
      this.info = info;
      return this;
    }

    public ObjectArrayDTO build() {
      ObjectArrayDTO objectArrayDTO = new ObjectArrayDTO(this);
      return objectArrayDTO;
    }
  }

  /**
  * Get idObject
  * @return idObject
  */
  @Schema(name = "idObject", required = false)
  public Integer getIdObject() {
    return idObject;
  }
  public void setIdObject(Integer idObject) {
    this.idObject = idObject;
  }

  /**
  * Get players
  * @return players
  */
  @Schema(name = "players", required = false)
  public List<String> getPlayers() {
    return players;
  }
  public void setPlayers(List<String> players) {
    this.players = players;
  }

  /**
  * Get info
  * @return info
  */
  @Schema(name = "info", required = false)
  public String getInfo() {
    return info;
  }
  public void setInfo(String info) {
    this.info = info;
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
    return Objects.equals(this.idObject, objectArrayDTO.idObject) && Objects.equals(this.players, objectArrayDTO.players) && Objects.equals(this.info, objectArrayDTO.info);
  }

  @Override
  public int hashCode() {
    return Objects.hash(idObject, players, info);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ObjectArrayDTO{");
    sb.append(" idObject:").append(toIndentedString(idObject)).append(",");
    sb.append(" players:").append(toIndentedString(players)).append(",");
    sb.append(" info:").append(toIndentedString(info)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
