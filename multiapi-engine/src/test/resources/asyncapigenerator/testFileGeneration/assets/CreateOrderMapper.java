package com.sngular.scsplugin.filegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrderMapper.CreateOrderMapperBuilder.class)
public class CreateOrderMapper {

  @JsonProperty(value ="order")
  private OrderMapper order;
  @JsonProperty(value ="waiter")
  private WaiterMapper waiter;

  private CreateOrderMapper(CreateOrderMapperBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrderMapper.CreateOrderMapperBuilder builder() {
    return new CreateOrderMapper.CreateOrderMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderMapperBuilder {

    private OrderMapper order;

    private WaiterMapper waiter;

    public CreateOrderMapper.CreateOrderMapperBuilder order(OrderMapper order) {
      this.order = order;
      return this;
    }

    public CreateOrderMapper.CreateOrderMapperBuilder waiter(WaiterMapper waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrderMapper build() {
      CreateOrderMapper createOrderMapper = new CreateOrderMapper(this);
      return createOrderMapper;
    }
  }

  /**
  * Get order
  * @return order
  */
  @Schema(name = "order", required = false)
  public OrderMapper getOrder() {
    return order;
  }
  public void setOrder(OrderMapper order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public WaiterMapper getWaiter() {
    return waiter;
  }
  public void setWaiter(WaiterMapper waiter) {
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
    CreateOrderMapper createOrderMapper = (CreateOrderMapper) o;
    return Objects.equals(this.order, createOrderMapper.order) && Objects.equals(this.waiter, createOrderMapper.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CreateOrderMapper{");
    sb.append(" order:").append(order).append(",");
    sb.append(" waiter:").append(waiter);
    sb.append("}");
    return sb.toString();
  }


}
