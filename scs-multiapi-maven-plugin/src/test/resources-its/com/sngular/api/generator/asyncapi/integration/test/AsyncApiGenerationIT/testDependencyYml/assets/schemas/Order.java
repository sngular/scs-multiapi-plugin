package com.sngular.apigenerator.asyncapi.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;

@JsonDeserialize(builder = Order.OrderBuilder.class)
public class Order {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;

  private Order(String ref, String clientRef, BigDecimal amount) {
    this.ref = ref;
    this.clientRef = clientRef;
    this.amount = amount;

  }

  private Order(OrderBuilder builder) {
    this.ref = builder.ref;
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;

  }

  public static Order.OrderBuilder builder() {
    return new Order.OrderBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderBuilder {

    private String ref;
    private String clientRef;
    private BigDecimal amount;

    public Order.OrderBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public Order.OrderBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    public Order.OrderBuilder amount(BigDecimal amount) {
      this.amount = amount;
      return this;
    }

    public Order build() {
      Order order = new Order(this);
      return order;
    }
  }

  /**
  * Get ref
  * @return ref
  */
  @Schema(name = "ref", required = false)
  public String getRef() {
    return ref;
  }
  public void setRef(String ref) {
    this.ref = ref;
  }

  /**
  * Get clientRef
  * @return clientRef
  */
  @Schema(name = "clientRef", required = false)
  public String getClientRef() {
    return clientRef;
  }
  public void setClientRef(String clientRef) {
    this.clientRef = clientRef;
  }

  /**
  * Get amount
  * @return amount
  */
  @Schema(name = "amount", required = false)
  public BigDecimal getAmount() {
    return amount;
  }
  public void setAmount(BigDecimal amount) {
    this.amount = amount;
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
    return Objects.equals(this.ref, order.ref) && Objects.equals(this.clientRef, order.clientRef) && Objects.equals(this.amount, order.amount);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, clientRef, amount);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    sb.append(" ref: ").append(toIndentedString(ref)).append("\n");
    sb.append(" clientRef: ").append(toIndentedString(clientRef)).append("\n");
    sb.append(" amount: ").append(toIndentedString(amount)).append("\n");
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
