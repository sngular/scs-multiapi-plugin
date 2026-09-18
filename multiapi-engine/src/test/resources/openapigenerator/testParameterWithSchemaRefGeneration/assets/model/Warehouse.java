package com.sngular.multifileplugin.testparameterschemaref.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Warehouse.WarehouseBuilder.class)
public class Warehouse {

  @JsonProperty(value ="id")
  private String id;

  private Warehouse(WarehouseBuilder builder) {
    this.id = builder.id;

  }

  public static Warehouse.WarehouseBuilder builder() {
    return new Warehouse.WarehouseBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WarehouseBuilder {

    private String id;

    public Warehouse.WarehouseBuilder id(String id) {
      this.id = id;
      return this;
    }

    public Warehouse build() {
      Warehouse warehouse = new Warehouse(this);
      return warehouse;
    }
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Warehouse warehouse = (Warehouse) o;
    return Objects.equals(this.id, warehouse.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Warehouse{");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
