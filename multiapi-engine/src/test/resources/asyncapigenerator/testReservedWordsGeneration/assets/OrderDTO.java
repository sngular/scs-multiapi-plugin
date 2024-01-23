package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = OrderDTO.OrderDTOBuilder.class)
public class OrderDTO {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="clientRef")
  private String clientRef;
  @JsonProperty(value ="amount")
  private BigDecimal amount;
  @JsonProperty(value ="new")
  private List<OrderLineDTO> _new = new ArrayList<OrderLineDTO>();

  private OrderDTO(OrderDTOBuilder builder) {
    this.ref = builder.ref;
    this.clientRef = builder.clientRef;
    this.amount = builder.amount;
    this._new.addAll(builder._new);

  }

  public static OrderDTO.OrderDTOBuilder builder() {
    return new OrderDTO.OrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderDTOBuilder {

    private String ref;

    private String clientRef;

    private BigDecimal amount;

    private List<OrderLineDTO> _new = new ArrayList<OrderLineDTO>();

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
  * Get new
  * @return new
  */
  @Schema(name = "new", required = false)
  public List<OrderLineDTO> getNew() {
    return _new;
  }
  public void setNew(List<OrderLineDTO> _new) {
    this._new = _new;
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
    return Objects.equals(this.ref, orderDTO.ref) && Objects.equals(this.clientRef, orderDTO.clientRef) && Objects.equals(this.amount, orderDTO.amount) && Objects.equals(this._new, orderDTO._new);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, clientRef, amount, _new);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderDTO{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" clientRef:").append(clientRef).append(",");
    sb.append(" amount:").append(amount).append(",");
    sb.append(" new:").append(_new);
    sb.append("}");
    return sb.toString();
  }


}
