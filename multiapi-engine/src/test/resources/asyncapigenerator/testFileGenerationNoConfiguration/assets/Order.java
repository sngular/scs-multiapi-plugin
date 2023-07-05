package com.sngular.apigenerator.asyncapi.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;

@JsonDeserialize(builder = Order.OrderBuilder.class)
public class Order {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="lines")
  private List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> lines = new ArrayList<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine>();

  private Order(String ref, String clientRef, BigDecimal amount, List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> lines) {
    this.ref = ref;
    this.clientRef = clientRef;
    this.amount = amount;
    this.lines = lines;

  }

  private Order(OrderBuilder builder) {
    this.ref = builder.ref;
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this.lines = builder.lines;

  }

  public static Order.OrderBuilder builder() {
    return new Order.OrderBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderBuilder {

    private String ref;
    private String clientRef;
    private BigDecimal amount;
    private List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> lines = new ArrayList<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine>();

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
    public Order.OrderBuilder lines(List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> lines) {
      if (!lines.isEmpty()) {
        this.lines.addAll(lines);
      }
      return this;
    }

    public Order.OrderBuilder line(com.sngular.apigenerator.asyncapi.model.schemas.OrderLine line) {
      if (line != null) {
        this.lines.add(line);
      }
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

  /**
  * Get lines
  * @return lines
  */
  @Schema(name = "lines", required = false)
  public List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> getLines() {
    return lines;
  }
  public void setLines(List<com.sngular.apigenerator.asyncapi.model.schemas.OrderLine> lines) {
    this.lines = lines;
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
    return Objects.equals(this.ref, order.ref) && Objects.equals(this.clientRef, order.clientRef) && Objects.equals(this.amount, order.amount) && Objects.equals(this.lines, order.lines);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, clientRef, amount, lines);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Order {\n");
    sb.append(" ref: ").append(toIndentedString(ref)).append("\n");
    sb.append(" clientRef: ").append(toIndentedString(clientRef)).append("\n");
    sb.append(" amount: ").append(toIndentedString(amount)).append("\n");
    sb.append(" lines: ").append(toIndentedString(lines)).append("\n");
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
