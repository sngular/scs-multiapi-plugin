package com.sngular.scsplugin.filegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.filegeneration.model.event.exception.ModelClassException;
import com.sngular.scsplugin.filegeneration.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLineDTO.OrderLineDTOBuilder.class)
public class OrderLineDTO {

  @JsonProperty(value ="products")
  private List<OrderProductDTO> products;
  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;

  private OrderLineDTO(OrderLineDTOBuilder builder) {
    this.products = builder.products;
    this.ref = builder.ref;

    validateRequiredAttributes();
  }

  public static OrderLineDTO.OrderLineDTOBuilder builder() {
    return new OrderLineDTO.OrderLineDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineDTOBuilder {

    private List<OrderProductDTO> products = new ArrayList<OrderProductDTO>();
    private String ref;

    public OrderLineDTO.OrderLineDTOBuilder products(List<OrderProductDTO> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLineDTO.OrderLineDTOBuilder product(OrderProductDTO product) {
      if (product != null) {
        this.products.add(product);
      }
      return this;
    }

    public OrderLineDTO.OrderLineDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderLineDTO build() {
      OrderLineDTO orderLineDTO = new OrderLineDTO(this);
      return orderLineDTO;
    }
  }

  @Schema(name = "products", required = false)
  public List<OrderProductDTO> getProducts() {
    return products;
  }
  public void setProducts(List<OrderProductDTO> products) {
    this.products = products;
  }

  @Schema(name = "ref", required = true)
  public String getRef() {
    return ref;
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
    return Objects.equals(this.products, orderLineDTO.products) && Objects.equals(this.ref, orderLineDTO.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(products, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineDTO{");
    sb.append(" products:").append(products).append(",");
    sb.append(" ref:").append(ref);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.ref)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderLineDTO");
    }
  }

}
