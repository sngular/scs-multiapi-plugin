package com.sngular.scsplugin.modelclass.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrderEventDTO.CreateOrderEventDTOBuilder.class)
public class CreateOrderEventDTO {

  @JsonProperty(value ="order")
  private OrderDTO order;
  @JsonProperty(value ="waiter")
  private WaiterDTO waiter;

  private CreateOrderEventDTO(CreateOrderEventDTOBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrderEventDTO.CreateOrderEventDTOBuilder builder() {
    return new CreateOrderEventDTO.CreateOrderEventDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderEventDTOBuilder {

    private OrderDTO order;
    private WaiterDTO waiter;

    public CreateOrderEventDTO.CreateOrderEventDTOBuilder order(OrderDTO order) {
      this.order = order;
      return this;
    }

    public CreateOrderEventDTO.CreateOrderEventDTOBuilder waiter(WaiterDTO waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrderEventDTO build() {
      CreateOrderEventDTO createOrderEventDTO = new CreateOrderEventDTO(this);
      return createOrderEventDTO;
    }
  }

  /**
  * Get order
  * @return order
  */
  @Schema(name = "order", required = false)
  public OrderDTO getOrder() {
    return order;
  }
  public void setOrder(OrderDTO order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public WaiterDTO getWaiter() {
    return waiter;
  }
  public void setWaiter(WaiterDTO waiter) {
    this.waiter = waiter;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CreateOrderEventDTO createOrderEventDTO = (CreateOrderEventDTO) o;
    return Objects.equals(this.order, createOrderEventDTO.order) && Objects.equals(this.waiter, createOrderEventDTO.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CreateOrderEventDTO{");
    sb.append(" order:").append(order).append(",");
    sb.append(" waiter:").append(waiter);
    sb.append("}");
    return sb.toString();
  }


}
