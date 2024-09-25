package com.sngular.apigenerator.asyncapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;

@JsonDeserialize(builder = Order.OrderBuilder.class)
public class Order {

  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="ref")
  private String ref;

  private Order(OrderBuilder builder) {
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this.ref = builder.ref;

  }

  public static Order.OrderBuilder builder() {
    return new Order.OrderBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderBuilder {

    private String clientRef;
    private BigDecimal amount;
    private String ref;

    public Order.OrderBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    public Order.OrderBuilder amount(BigDecimal amount) {
      this.amount = amount;
      return this;
    }

    public Order.OrderBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public Order build() {
      Order order = new Order(this);
      return order;
    }
  }

  @Schema(name = "clientRef", required = false)
  public String getClientRef() {
    return clientRef;
  }
  public void setClientRef(String clientRef) {
    this.clientRef = clientRef;
  }

  @Schema(name = "amount", required = false)
  public BigDecimal getAmount() {
    return amount;
  }
  public void setAmount(BigDecimal amount) {
    this.amount = amount;
  }

  @Schema(name = "ref", required = false)
  public String getRef() {
    return ref;
  }
  public void setRef(String ref) {
    this.ref = ref;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Order order = (Order) o;
    return Objects.equals(this.clientRef, order.clientRef) && Objects.equals(this.amount, order.amount) && Objects.equals(this.ref, order.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientRef, amount, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Order{");
    sb.append(" clientRef:").append(clientRef).append(",");
    sb.append(" amount:").append(amount).append(",");
    sb.append(" ref:").append(ref);
    sb.append("}");
    return sb.toString();
  }


}
