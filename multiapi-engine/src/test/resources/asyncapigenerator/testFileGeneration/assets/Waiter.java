package net.coru.scsplugin.filegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class Waiter {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="timestamp")
  private String timestamp;
  @JsonProperty(value ="table")
  private String table;

  private Waiter(String ref, String timestamp, String table) {
    this.ref = ref;
    this.timestamp = timestamp;
    this.table = table;

  }

  private Waiter(WaiterBuilder builder) {
    this.ref = builder.ref;
    this.timestamp = builder.timestamp;
    this.table = builder.table;

  }

  public static Waiter.WaiterBuilder builder() {
    return new Waiter.WaiterBuilder();
  }

  public static class WaiterBuilder {

    private String ref;
    private String timestamp;
    private String table;

    public Waiter.WaiterBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public Waiter.WaiterBuilder timestamp(String timestamp) {
      this.timestamp = timestamp;
      return this;
    }

    public Waiter.WaiterBuilder table(String table) {
      this.table = table;
      return this;
    }

    public Waiter build() {
      Waiter waiter = new Waiter(this);
      return waiter;
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
    Waiter waiter = (Waiter) o;
    return Objects.equals(this.ref, waiter.ref) && Objects.equals(this.timestamp, waiter.timestamp) && Objects.equals(this.table, waiter.table);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, timestamp, table);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Waiter {\n");
    sb.append(" ref: ").append(toIndentedString(ref)).append("\n");
    sb.append(" timestamp: ").append(toIndentedString(timestamp)).append("\n");
    sb.append(" table: ").append(toIndentedString(table)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }



}
