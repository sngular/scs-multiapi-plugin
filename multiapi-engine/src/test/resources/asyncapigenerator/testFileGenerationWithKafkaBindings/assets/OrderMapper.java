package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;

@JsonDeserialize(builder = OrderMapper.OrderMapperBuilder.class)
public class OrderMapper {

  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="lines")
  private List<OrderLineMapper> lines;
  @JsonProperty(value ="ref")
  private String ref;

  private OrderMapper(OrderMapperBuilder builder) {
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this.lines = builder.lines;
    this.ref = builder.ref;

  }

  public static OrderMapper.OrderMapperBuilder builder() {
    return new OrderMapper.OrderMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderMapperBuilder {

    private String clientRef;
    private BigDecimal amount;
    private List<OrderLineMapper> lines = new ArrayList<OrderLineMapper>();
    private String ref;

    public OrderMapper.OrderMapperBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    public OrderMapper.OrderMapperBuilder amount(BigDecimal amount) {
      this.amount = amount;
      return this;
    }

    public OrderMapper.OrderMapperBuilder lines(List<OrderLineMapper> lines) {
      if (!lines.isEmpty()) {
        this.lines.addAll(lines);
      }
      return this;
    }

    public OrderMapper.OrderMapperBuilder line(OrderLineMapper line) {
      if (line != null) {
        this.lines.add(line);
      }
      return this;
    }

    public OrderMapper.OrderMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderMapper build() {
      OrderMapper orderMapper = new OrderMapper(this);
      return orderMapper;
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

  @Schema(name = "lines", required = false)
  public List<OrderLineMapper> getLines() {
    return lines;
  }
  public void setLines(List<OrderLineMapper> lines) {
    this.lines = lines;
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
    OrderMapper orderMapper = (OrderMapper) o;
    return Objects.equals(this.clientRef, orderMapper.clientRef) && Objects.equals(this.amount, orderMapper.amount) && Objects.equals(this.lines, orderMapper.lines) && Objects.equals(this.ref, orderMapper.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientRef, amount, lines, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderMapper{");
    sb.append(" clientRef:").append(clientRef).append(",");
    sb.append(" amount:").append(amount).append(",");
    sb.append(" lines:").append(lines).append(",");
    sb.append(" ref:").append(ref);
    sb.append("}");
    return sb.toString();
  }


}
