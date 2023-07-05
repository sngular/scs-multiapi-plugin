package com.sngular.apigenerator.asyncapi.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = CreateOrder.CreateOrderBuilder.class)
public class CreateOrder {

  @JsonProperty(value ="order")
  private com.sngular.apigenerator.asyncapi.model.schemas.Order order;
  @JsonProperty(value ="waiter")
  private com.sngular.apigenerator.asyncapi.model.schemas.Waiter waiter;

  private CreateOrder(com.sngular.apigenerator.asyncapi.model.schemas.Order order, com.sngular.apigenerator.asyncapi.model.schemas.Waiter waiter) {
    this.order = order;
    this.waiter = waiter;

  }

  private CreateOrder(CreateOrderBuilder builder) {
    this.order = builder.order;
    this.waiter = builder.waiter;

  }

  public static CreateOrder.CreateOrderBuilder builder() {
    return new CreateOrder.CreateOrderBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CreateOrderBuilder {

    private com.sngular.apigenerator.asyncapi.model.schemas.Order order;
    private com.sngular.apigenerator.asyncapi.model.schemas.Waiter waiter;

    public CreateOrder.CreateOrderBuilder order(com.sngular.apigenerator.asyncapi.model.schemas.Order order) {
      this.order = order;
      return this;
    }

    public CreateOrder.CreateOrderBuilder waiter(com.sngular.apigenerator.asyncapi.model.schemas.Waiter waiter) {
      this.waiter = waiter;
      return this;
    }

    public CreateOrder build() {
      CreateOrder createOrder = new CreateOrder(this);
      return createOrder;
    }
  }

  /**
  * Get order
  * @return order
  */
  @Schema(name = "order", required = false)
  public com.sngular.apigenerator.asyncapi.model.schemas.Order getOrder() {
    return order;
  }
  public void setOrder(com.sngular.apigenerator.asyncapi.model.schemas.Order order) {
    this.order = order;
  }

  /**
  * Get waiter
  * @return waiter
  */
  @Schema(name = "waiter", required = false)
  public com.sngular.apigenerator.asyncapi.model.schemas.Waiter getWaiter() {
    return waiter;
  }
  public void setWaiter(com.sngular.apigenerator.asyncapi.model.schemas.Waiter waiter) {
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
    CreateOrder createOrder = (CreateOrder) o;
    return Objects.equals(this.order, createOrder.order) && Objects.equals(this.waiter, createOrder.waiter);
  }

  @Override
  public int hashCode() {
    return Objects.hash(order, waiter);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateOrder {\n");
    sb.append(" order: ").append(toIndentedString(order)).append("\n");
    sb.append(" waiter: ").append(toIndentedString(waiter)).append("\n");
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
