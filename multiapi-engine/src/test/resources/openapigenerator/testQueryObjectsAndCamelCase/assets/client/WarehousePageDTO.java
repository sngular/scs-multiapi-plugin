package com.sngular.multifileplugin.queryobjects.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = WarehousePageDTO.WarehousePageDTOBuilder.class)
public class WarehousePageDTO {

  @JsonProperty(value ="warehouse_names")
  private List<String> warehouse_names;
  @JsonProperty(value ="total_items")
  private Long total_items;

  private WarehousePageDTO(WarehousePageDTOBuilder builder) {
    this.warehouse_names = builder.warehouse_names;
    this.total_items = builder.total_items;

  }

  public static WarehousePageDTO.WarehousePageDTOBuilder builder() {
    return new WarehousePageDTO.WarehousePageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WarehousePageDTOBuilder {

    private List<String> warehouse_names = new ArrayList<String>();
    private Long total_items;

    public WarehousePageDTO.WarehousePageDTOBuilder warehouse_names(List<String> warehouse_names) {
      if (Objects.nonNull(warehouse_names) && !warehouse_names.isEmpty()) {
        this.warehouse_names.addAll(warehouse_names);
      }
      return this;
    }

    public WarehousePageDTO.WarehousePageDTOBuilder warehouse_name(String warehouse_name) {
      if (Objects.nonNull(warehouse_name)) {
        this.warehouse_names.add(warehouse_name);
      }
      return this;
    }

    public WarehousePageDTO.WarehousePageDTOBuilder total_items(Long total_items) {
      this.total_items = total_items;
      return this;
    }

    public WarehousePageDTO build() {
      WarehousePageDTO warehousePageDTO = new WarehousePageDTO(this);
      return warehousePageDTO;
    }
  }

  @Schema(name = "warehouse_names", required = false)
  public List<String> getWarehouse_names() {
    return warehouse_names;
  }
  public void setWarehouse_names(List<String> warehouse_names) {
    this.warehouse_names = warehouse_names;
  }

  @Schema(name = "total_items", required = false)
  public Long getTotal_items() {
    return total_items;
  }
  public void setTotal_items(Long total_items) {
    this.total_items = total_items;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    WarehousePageDTO warehousePageDTO = (WarehousePageDTO) o;
    return Objects.equals(this.warehouse_names, warehousePageDTO.warehouse_names) && Objects.equals(this.total_items, warehousePageDTO.total_items);
  }

  @Override
  public int hashCode() {
    return Objects.hash(warehouse_names, total_items);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("WarehousePageDTO{");
    sb.append(" warehouse_names:").append(warehouse_names).append(",");
    sb.append(" total_items:").append(total_items);
    sb.append("}");
    return sb.toString();
  }


}
