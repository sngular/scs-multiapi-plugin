package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.exception.ModelClassException;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLineDTO.OrderLineDTOBuilder.class)
public class OrderLineDTO {

  @JsonProperty(value ="_continue")
  private List<String> _continue = new ArrayList<String>();
  @JsonProperty(value ="_byte")
  @NotNull
  private final BigDecimal _byte;

  private OrderLineDTO(OrderLineDTOBuilder builder) {
    this._continue = builder._continue;
    this._byte = builder._byte;

    validateRequiredAttributes();
  }

  public static OrderLineDTO.OrderLineDTOBuilder builder() {
    return new OrderLineDTO.OrderLineDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineDTOBuilder {

    private List<String> _continue = new ArrayList<String>();
    private BigDecimal _byte;
    public OrderLineDTO.OrderLineDTOBuilder _continue(List<String> _continue) {
      if (!_continue.isEmpty()) {
        this._continue.addAll(_continue);
      }
      return this;
    }

    public OrderLineDTO.OrderLineDTOBuilder _continue(String _continue) {
      if (_continue != null) {
        this._continue.add(_continue);
      }
      return this;
    }

    public OrderLineDTO.OrderLineDTOBuilder _byte(BigDecimal _byte) {
      this._byte = _byte;
      return this;
    }

    public OrderLineDTO build() {
      OrderLineDTO orderLineDTO = new OrderLineDTO(this);
      return orderLineDTO;
    }
  }

  @Schema(name = "continue", required = false)
  public List<String> getContinue() {
    return _continue;
  }
  public void setContinue(List<String> _continue) {
    this._continue = _continue;
  }

  @Schema(name = "byte", required = true)
  public BigDecimal getByte() {
    return _byte;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderLineDTO orderLineDTO = (OrderLineDTO) o;
    return Objects.equals(this._continue, orderLineDTO._continue) && Objects.equals(this._byte, orderLineDTO._byte);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_continue, _byte);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineDTO{");
    sb.append(" continue:").append(_continue).append(",");
    sb.append(" byte:").append(_byte);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this._byte)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderLineDTO");
    }
  }

}
