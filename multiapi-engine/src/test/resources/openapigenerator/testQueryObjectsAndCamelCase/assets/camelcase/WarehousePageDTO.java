package com.sngular.multifileplugin.camelcase.model;

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
  private List<String> warehouseNames;
  @JsonProperty(value ="total_items")
  private Long totalItems;

  private WarehousePageDTO(WarehousePageDTOBuilder builder) {
    this.warehouseNames = builder.warehouseNames;
    this.totalItems = builder.totalItems;

  }

  public static WarehousePageDTO.WarehousePageDTOBuilder builder() {
    return new WarehousePageDTO.WarehousePageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class WarehousePageDTOBuilder {

    private List<String> warehouseNames = new ArrayList<String>();
    private Long totalItems;

    @JsonProperty(value ="warehouse_names")
    public WarehousePageDTO.WarehousePageDTOBuilder warehouseNames(List<String> warehouseNames) {
      if (Objects.nonNull(warehouseNames) && !warehouseNames.isEmpty()) {
        this.warehouseNames.addAll(warehouseNames);
      }
      return this;
    }

    public WarehousePageDTO.WarehousePageDTOBuilder warehouseName(String warehouseName) {
      if (Objects.nonNull(warehouseName)) {
        this.warehouseNames.add(warehouseName);
      }
      return this;
    }

    @JsonProperty(value ="total_items")
    public WarehousePageDTO.WarehousePageDTOBuilder totalItems(Long totalItems) {
      this.totalItems = totalItems;
      return this;
    }

    public WarehousePageDTO build() {
      WarehousePageDTO warehousePageDTO = new WarehousePageDTO(this);
      return warehousePageDTO;
    }
  }

  @Schema(name = "warehouse_names", required = false)
  public List<String> getWarehouseNames() {
    return warehouseNames;
  }
  public void setWarehouseNames(List<String> warehouseNames) {
    this.warehouseNames = warehouseNames;
  }

  @Schema(name = "total_items", required = false)
  public Long getTotalItems() {
    return totalItems;
  }
  public void setTotalItems(Long totalItems) {
    this.totalItems = totalItems;
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
    return Objects.equals(this.warehouseNames, warehousePageDTO.warehouseNames) && Objects.equals(this.totalItems, warehousePageDTO.totalItems);
  }

  @Override
  public int hashCode() {
    return Objects.hash(warehouseNames, totalItems);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("WarehousePageDTO{");
    sb.append(" warehouse_names:").append(warehouseNames).append(",");
    sb.append(" total_items:").append(totalItems);
    sb.append("}");
    return sb.toString();
  }


}
