package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = WaiterMapper.WaiterMapperBuilder.class)
public class WaiterMapper {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="timestamp")
  private String timestamp;
  @JsonProperty(value ="table")
  private String table;

  private WaiterMapper(String ref, String timestamp, String table) {
    this.ref = ref;
    this.timestamp = timestamp;
    this.table = table;

  }

  private WaiterMapper(WaiterMapperBuilder builder) {
    this.ref = builder.ref;
    this.timestamp = builder.timestamp;
    this.table = builder.table;

  }

  public static WaiterMapper.WaiterMapperBuilder builder() {
    return new WaiterMapper.WaiterMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WaiterMapperBuilder {

    private String ref;
    private String timestamp;
    private String table;

    public WaiterMapper.WaiterMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public WaiterMapper.WaiterMapperBuilder timestamp(String timestamp) {
      this.timestamp = timestamp;
      return this;
    }

    public WaiterMapper.WaiterMapperBuilder table(String table) {
      this.table = table;
      return this;
    }

    public WaiterMapper build() {
      WaiterMapper waiterMapper = new WaiterMapper(this);
      return waiterMapper;
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
    WaiterMapper waiterMapper = (WaiterMapper) o;
    return Objects.equals(this.ref, waiterMapper.ref) && Objects.equals(this.timestamp, waiterMapper.timestamp) && Objects.equals(this.table, waiterMapper.table);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, timestamp, table);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("WaiterMapper{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" timestamp:").append(timestamp).append(",");
    sb.append(" table:").append(table);
    sb.append("}");
    return sb.toString();
  }


}
