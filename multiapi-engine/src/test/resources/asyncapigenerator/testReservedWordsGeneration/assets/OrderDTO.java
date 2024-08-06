package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;

@JsonDeserialize(builder = OrderDTO.OrderDTOBuilder.class)
public class OrderDTO {

  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="_new")
  private List<OrderLineDTO> _new = new ArrayList<OrderLineDTO>();
  @JsonProperty(value ="ref")
  private String ref;

  private OrderDTO(OrderDTOBuilder builder) {
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this._new = builder._new;
    this.ref = builder.ref;

  }

  public static OrderDTO.OrderDTOBuilder builder() {
    return new OrderDTO.OrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderDTOBuilder {

    private String clientRef;
    private BigDecimal amount;
    private List<OrderLineDTO> _new = new ArrayList<OrderLineDTO>();
    private String ref;

    public OrderDTO.OrderDTOBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    public OrderDTO.OrderDTOBuilder amount(BigDecimal amount) {
      this.amount = amount;
      return this;
    }
    public OrderDTO.OrderDTOBuilder _new(List<OrderLineDTO> _new) {
      if (!_new.isEmpty()) {
        this._new.addAll(_new);
      }
      return this;
    }

    public OrderDTO.OrderDTOBuilder _new(OrderLineDTO _new) {
      if (_new != null) {
        this._new.add(_new);
      }
      return this;
    }

    public OrderDTO.OrderDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderDTO build() {
      OrderDTO orderDTO = new OrderDTO(this);
      return orderDTO;
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

  @Schema(name = "new", required = false)
  public List<OrderLineDTO> getNew() {
    return _new;
  }
  public void setNew(List<OrderLineDTO> _new) {
    this._new = _new;
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
    OrderDTO orderDTO = (OrderDTO) o;
    return Objects.equals(this.clientRef, orderDTO.clientRef) && Objects.equals(this.amount, orderDTO.amount) && Objects.equals(this._new, orderDTO._new) && Objects.equals(this.ref, orderDTO.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(clientRef, amount, _new, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderDTO{");
    sb.append(" clientRef:").append(clientRef).append(",");
    sb.append(" amount:").append(amount).append(",");
    sb.append(" new:").append(_new).append(",");
    sb.append(" ref:").append(ref);
    sb.append("}");
    return sb.toString();
  }


}
