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

  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;
  @JsonProperty(value ="products")
  @NotNull
  private final List<OrderProductDTO> products;

  private OrderLineDTO(String ref, List<OrderProductDTO> products) {
    this.ref = ref;
    this.products = products;

    validateRequiredAttributes();
  }

  private OrderLineDTO(OrderLineDTOBuilder builder) {
    this.ref = builder.ref;
    this.products = builder.products;

    validateRequiredAttributes();
  }

  public static OrderLineDTO.OrderLineDTOBuilder builder() {
    return new OrderLineDTO.OrderLineDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineDTOBuilder {

    private String ref;
    private List<OrderProductDTO> products = new ArrayList<OrderProductDTO>();

    public OrderLineDTO.OrderLineDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

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

    public OrderLineDTO build() {
      OrderLineDTO orderLineDTO = new OrderLineDTO(this);
      return orderLineDTO;
    }
  }

  /**
  * Get ref
  * @return ref
  */
  @Schema(name = "ref", required = true)
  public String getRef() {
    return ref;
  }

  /**
  * Get products
  * @return products
  */
  @Schema(name = "products", required = true)
  public List<OrderProductDTO> getProducts() {
    return products;
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
    return Objects.equals(this.ref, orderLineDTO.ref) && Objects.equals(this.products, orderLineDTO.products);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, products);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineDTO{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" products:").append(products);
    sb.append("}");
    return sb.toString();
  }


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.ref)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.products)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderLineDTO");
    }
  }
}
