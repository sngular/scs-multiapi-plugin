package com.sngular.scsplugin.filegeneration.model.event.schema;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import com.sngular.scsplugin.filegeneration.model.event.schema.OrderLineDTO;

@JsonDeserialize(builder = OrderDTO.OrderDTOBuilder.class)
public class OrderDTO {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="lines")
  private List<OrderLineDTO> lines = new ArrayList<OrderLineDTO>();

  private OrderDTO(String ref, String clientRef, BigDecimal amount, List<OrderLineDTO> lines) {
    this.ref = ref;
    this.clientRef = clientRef;
    this.amount = amount;
    this.lines = lines;

  }

  private OrderDTO(OrderDTOBuilder builder) {
    this.ref = builder.ref;
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this.lines = builder.lines;

  }

  public static OrderDTO.OrderDTOBuilder builder() {
    return new OrderDTO.OrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderDTOBuilder {

    private String ref;
    private String clientRef;
    private BigDecimal amount;
    private List<OrderLineDTO> lines = new ArrayList<OrderLineDTO>();

    public OrderDTO.OrderDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderDTO.OrderDTOBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    public OrderDTO.OrderDTOBuilder amount(BigDecimal amount) {
      this.amount = amount;
      return this;
    }
    public OrderDTO.OrderDTOBuilder lines(List<OrderLineDTO> lines) {
      if (!lines.isEmpty()) {
        this.lines.addAll(lines);
      }
      return this;
    }

    public OrderDTO.OrderDTOBuilder line(OrderLineDTO line) {
      if (line != null) {
        this.lines.add(line);
      }
      return this;
    }

    public OrderDTO build() {
      OrderDTO orderDTO = new OrderDTO(this);
      return orderDTO;
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
  public List<OrderLineDTO> getLines() {
    return lines;
  }
  public void setLines(List<OrderLineDTO> lines) {
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
    OrderDTO orderDTO = (OrderDTO) o;
    return Objects.equals(this.ref, orderDTO.ref) && Objects.equals(this.clientRef, orderDTO.clientRef) && Objects.equals(this.amount, orderDTO.amount) && Objects.equals(this.lines, orderDTO.lines);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, clientRef, amount, lines);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderDTO {\n");
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
