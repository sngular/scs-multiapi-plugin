package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrderDTO.CreateOrderDTOBuilder.class)
public class CreateOrderDTO {

  @JsonProperty(value ="waiter")
  private WaiterDTO waiter;
  @JsonProperty(value ="order")
  private OrderDTO order;

  private CreateOrderDTO(CreateOrderDTOBuilder builder) {
    this.waiter = builder.waiter;
    this.order = builder.order;

  }

  public static CreateOrderDTO.CreateOrderDTOBuilder builder() {
    return new CreateOrderDTO.CreateOrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderDTOBuilder {

    private WaiterDTO waiter;
    private OrderDTO order;
    public CreateOrderDTO.CreateOrderDTOBuilder waiter(WaiterDTO waiter) {
      this.waiter = waiter;
      return this;
    }
    public CreateOrderDTO.CreateOrderDTOBuilder order(OrderDTO order) {
      this.order = order;
      return this;
    }

    public CreateOrderDTO build() {
      CreateOrderDTO createOrderDTO = new CreateOrderDTO(this);
      return createOrderDTO;
    }
  }

  @Schema(name = "waiter", required = false)
  public WaiterDTO getWaiter() {
    return waiter;
  }
  public void setWaiter(WaiterDTO waiter) {
    this.waiter = waiter;
  }

  @Schema(name = "order", required = false)
  public OrderDTO getOrder() {
    return order;
  }
  public void setOrder(OrderDTO order) {
    this.order = order;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CreateOrderDTO createOrderDTO = (CreateOrderDTO) o;
    return Objects.equals(this.waiter, createOrderDTO.waiter) && Objects.equals(this.order, createOrderDTO.order);
  }

  @Override
  public int hashCode() {
    return Objects.hash(waiter, order);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CreateOrderDTO{");
    sb.append(" waiter:").append(waiter).append(",");
    sb.append(" order:").append(order);
    sb.append("}");
    return sb.toString();
  }


}
