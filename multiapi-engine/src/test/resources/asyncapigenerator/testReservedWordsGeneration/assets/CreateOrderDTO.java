package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrderDTO.CreateOrderDTOBuilder.class)
public class CreateOrderDTO {

  @JsonProperty(value ="order")
  private OrderDTO order;
  @JsonProperty(value ="waiter")
  private WaiterDTO waiter;

  private CreateOrderDTO(CreateOrderDTOBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrderDTO.CreateOrderDTOBuilder builder() {
    return new CreateOrderDTO.CreateOrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderDTOBuilder {

    private OrderDTO order;
    private WaiterDTO waiter;

    public CreateOrderDTO.CreateOrderDTOBuilder order(OrderDTO order) {
      this.order = order;
      return this;
    }

    public CreateOrderDTO.CreateOrderDTOBuilder waiter(WaiterDTO waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrderDTO build() {
      CreateOrderDTO createOrderDTO = new CreateOrderDTO(this);
      return createOrderDTO;
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
    CreateOrderDTO createOrderDTO = (CreateOrderDTO) o;
    return Objects.equals(this.order, createOrderDTO.order) && Objects.equals(this.waiter, createOrderDTO.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CreateOrderDTO{");
    sb.append(" order:").append(order).append(",");
    sb.append(" waiter:").append(waiter);
    sb.append("}");
    return sb.toString();
  }


}
