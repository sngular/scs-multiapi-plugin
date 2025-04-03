package com.sngular.scsplugin.rarecharsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = WaiterDTO.WaiterDTOBuilder.class)
public class WaiterDTO {

  @JsonProperty(value ="_abstract")
  private String _abstract;
  @JsonProperty(value ="_new")
  private Boolean _new;
  @JsonProperty(value ="_private")
  private String _private;

  private WaiterDTO(WaiterDTOBuilder builder) {
    this._abstract = builder._abstract;
    this._new = builder._new;
    this._private = builder._private;

  }

  public static WaiterDTO.WaiterDTOBuilder builder() {
    return new WaiterDTO.WaiterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WaiterDTOBuilder {

    private String _abstract;
    private Boolean _new;
    private String _private;

    public WaiterDTO.WaiterDTOBuilder _abstract(String _abstract) {
      this._abstract = _abstract;
      return this;
    }

    public WaiterDTO.WaiterDTOBuilder _new(Boolean _new) {
      this._new = _new;
      return this;
    }

    public WaiterDTO.WaiterDTOBuilder _private(String _private) {
      this._private = _private;
      return this;
    }

    public WaiterDTO build() {
      WaiterDTO waiterDTO = new WaiterDTO(this);
      return waiterDTO;
    }
  }

  @Schema(name = "abstract", required = false)
  public String getAbstract() {
    return _abstract;
  }
  public void setAbstract(String _abstract) {
    this._abstract = _abstract;
  }

  @Schema(name = "new", required = false)
  public Boolean getNew() {
    return _new;
  }
  public void setNew(Boolean _new) {
    this._new = _new;
  }

  @Schema(name = "private", required = false)
  public String getPrivate() {
    return _private;
  }
  public void setPrivate(String _private) {
    this._private = _private;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    WaiterDTO waiterDTO = (WaiterDTO) o;
    return Objects.equals(this._abstract, waiterDTO._abstract) && Objects.equals(this._new, waiterDTO._new) && Objects.equals(this._private, waiterDTO._private);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_abstract, _new, _private);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("WaiterDTO{");
    sb.append(" abstract:").append(_abstract).append(",");
    sb.append(" new:").append(_new).append(",");
    sb.append(" private:").append(_private);
    sb.append("}");
    return sb.toString();
  }


}
