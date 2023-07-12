package com.sngular.scsplugin.filegeneration.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrderMapper.CreateOrderMapperBuilder.class)
public class CreateOrderMapper {

  @JsonProperty(value ="order")
  private com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper order;
  @JsonProperty(value ="waiter")
  private com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper waiter;

  private CreateOrderMapper(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper order, com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper waiter) {
    this.order = order;
    this.waiter = waiter;

  }

  private CreateOrderMapper(CreateOrderMapperBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrderMapper.CreateOrderMapperBuilder builder() {
    return new CreateOrderMapper.CreateOrderMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderMapperBuilder {

    private com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper order;
    private com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper waiter;

    public CreateOrderMapper.CreateOrderMapperBuilder order(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper order) {
      this.order = order;
      return this;
    }

    public CreateOrderMapper.CreateOrderMapperBuilder waiter(com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper waiter) {
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
  public com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper getOrder() {
    return order;
  }
  public void setOrder(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderMapper order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper getWaiter() {
    return waiter;
  }
  public void setWaiter(com.sngular.scsplugin.filegeneration.model.event.schemas.WaiterMapper waiter) {
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
    sb.append(" order:").append(toIndentedString(order)).append(",");
    sb.append(" waiter:").append(toIndentedString(waiter)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
