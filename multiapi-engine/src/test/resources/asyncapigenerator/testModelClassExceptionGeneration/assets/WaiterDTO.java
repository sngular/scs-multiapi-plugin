package com.sngular.scsplugin.modelclass.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = WaiterDTO.WaiterDTOBuilder.class)
public class WaiterDTO {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="timestamp")
  private String timestamp;
  @JsonProperty(value ="table")
  private String table;

  private WaiterDTO(WaiterDTOBuilder builder) {
    this.ref = builder.ref;
    this.timestamp = builder.timestamp;
    this.table = builder.table;

  }

  public static WaiterDTO.WaiterDTOBuilder builder() {
    return new WaiterDTO.WaiterDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WaiterDTOBuilder {

    private String ref;

    private String timestamp;

    private String table;

    public WaiterDTO.WaiterDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public WaiterDTO.WaiterDTOBuilder timestamp(String timestamp) {
      this.timestamp = timestamp;
      return this;
    }

    public WaiterDTO.WaiterDTOBuilder table(String table) {
      this.table = table;
      return this;
    }

    public WaiterDTO build() {
      WaiterDTO waiterDTO = new WaiterDTO(this);
      return waiterDTO;
    }
  }

  /**
  * Get ref
  * @return ref
  */
  @Schema(name = "ref", required = false)
  public String getRef() {
    return ref;
  }
  public void setRef(String ref) {
    this.ref = ref;
  }

  /**
  * Get timestamp
  * @return timestamp
  */
  @Schema(name = "timestamp", required = false)
  public String getTimestamp() {
    return timestamp;
  }
  public void setTimestamp(String timestamp) {
    this.timestamp = timestamp;
  }

  /**
  * Get table
  * @return table
  */
  @Schema(name = "table", required = false)
  public String getTable() {
    return table;
  }
  public void setTable(String table) {
    this.table = table;
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
    return Objects.equals(this.ref, waiterDTO.ref) && Objects.equals(this.timestamp, waiterDTO.timestamp) && Objects.equals(this.table, waiterDTO.table);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, timestamp, table);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("WaiterDTO{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" timestamp:").append(timestamp).append(",");
    sb.append(" table:").append(table);
    sb.append("}");
    return sb.toString();
  }


}
