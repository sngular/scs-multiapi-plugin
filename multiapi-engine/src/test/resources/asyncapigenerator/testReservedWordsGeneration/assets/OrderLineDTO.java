package com.sngular.scsplugin.reservedwordsgeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.exception.ModelClassException;
import com.sngular.scsplugin.reservedwordsgeneration.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLineDTO.OrderLineDTOBuilder.class)
public class OrderLineDTO {

  @JsonProperty(value ="byte")
  @NotNull
  private final BigDecimal _byte;
  @JsonProperty(value ="continue")
  private List<String> _continue = new ArrayList<String>();

  private OrderLineDTO(BigDecimal _byte, List<String> _continue) {
    this._byte = _byte;
    this._continue = _continue;

    validateRequiredAttributes();
  }

  private OrderLineDTO(OrderLineDTOBuilder builder) {
    this._byte = builder._byte;
    this._continue = builder._continue;

    validateRequiredAttributes();
  }

  public static OrderLineDTO.OrderLineDTOBuilder builder() {
    return new OrderLineDTO.OrderLineDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineDTOBuilder {

    private BigDecimal _byte;
    private List<String> _continue = new ArrayList<String>();

    public OrderLineDTO.OrderLineDTOBuilder _byte(BigDecimal _byte) {
      this._byte = _byte;
      return this;
    }

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

    public OrderLineDTO build() {
      OrderLineDTO orderLineDTO = new OrderLineDTO(this);
      return orderLineDTO;
    }
  }

  /**
  * Get byte
  * @return byte
  */
  @Schema(name = "byte", required = true)
  public BigDecimal getByte() {
    return _byte;
  }

  /**
  * Get continue
  * @return continue
  */
  @Schema(name = "continue", required = false)
  public List<String> getContinue() {
    return _continue;
  }
  public void setContinue(List<String> _continue) {
    this._continue = _continue;
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
    return Objects.equals(this._byte, orderLineDTO._byte) && Objects.equals(this._continue, orderLineDTO._continue);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_byte, _continue);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineDTO{");
    sb.append(" byte:").append(_byte).append(",");
    sb.append(" continue:").append(_continue);
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
