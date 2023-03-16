package com.sngular.scsplugin.filegeneration.model.event.schema;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import com.sngular.scsplugin.filegeneration.model.event.schema.OrderLineMapper;

@JsonDeserialize(builder = OrderMapper.OrderMapperBuilder.class)
public class OrderMapper {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="lines")
  private List<OrderLineMapper> lines = new ArrayList<OrderLineMapper>();

  private OrderMapper(String ref, String clientRef, BigDecimal amount, List<OrderLineMapper> lines) {
    this.ref = ref;
    this.clientRef = clientRef;
    this.amount = amount;
    this.lines = lines;

  }

  private OrderMapper(OrderMapperBuilder builder) {
    this.ref = builder.ref;
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this.lines = builder.lines;

  }

  public static OrderMapper.OrderMapperBuilder builder() {
    return new OrderMapper.OrderMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderMapperBuilder {

    private String ref;
    private String clientRef;
    private BigDecimal amount;
    private List<OrderLineMapper> lines = new ArrayList<OrderLineMapper>();

    public OrderMapper.OrderMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

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

    public OrderMapper build() {
      OrderMapper orderMapper = new OrderMapper(this);
      return orderMapper;
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
  public List<OrderLineMapper> getLines() {
    return lines;
  }
  public void setLines(List<OrderLineMapper> lines) {
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
    OrderMapper orderMapper = (OrderMapper) o;
    return Objects.equals(this.ref, orderMapper.ref) && Objects.equals(this.clientRef, orderMapper.clientRef) && Objects.equals(this.amount, orderMapper.amount) && Objects.equals(this.lines, orderMapper.lines);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, clientRef, amount, lines);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderMapper {\n");
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
