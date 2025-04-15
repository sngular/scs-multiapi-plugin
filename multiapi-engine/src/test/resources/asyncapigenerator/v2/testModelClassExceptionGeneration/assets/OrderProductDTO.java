package com.sngular.scsplugin.modelclass.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.modelclass.model.event.exception.ModelClassException;
import com.sngular.scsplugin.modelclass.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderProductDTO.OrderProductDTOBuilder.class)
public class OrderProductDTO {

  @JsonProperty(value ="productRef")
  @NotNull
  private final String productRef;
  @JsonProperty(value ="price")
  @NotNull
  private final String price;
  @JsonProperty(value ="quantity")
  @NotNull
  private final String quantity;
  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;

  private OrderProductDTO(OrderProductDTOBuilder builder) {
    this.productRef = builder.productRef;
    this.price = builder.price;
    this.quantity = builder.quantity;
    this.ref = builder.ref;

    validateRequiredAttributes();
  }

  public static OrderProductDTO.OrderProductDTOBuilder builder() {
    return new OrderProductDTO.OrderProductDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderProductDTOBuilder {

    private String productRef;
    private String price;
    private String quantity;
    private String ref;

    public OrderProductDTO.OrderProductDTOBuilder productRef(String productRef) {
      this.productRef = productRef;
      return this;
    }

    public OrderProductDTO.OrderProductDTOBuilder price(String price) {
      this.price = price;
      return this;
    }

    public OrderProductDTO.OrderProductDTOBuilder quantity(String quantity) {
      this.quantity = quantity;
      return this;
    }

    public OrderProductDTO.OrderProductDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderProductDTO build() {
      OrderProductDTO orderProductDTO = new OrderProductDTO(this);
      return orderProductDTO;
    }
  }

  @Schema(name = "productRef", required = true)
  public String getProductRef() {
    return productRef;
  }

  @Schema(name = "price", required = true)
  public String getPrice() {
    return price;
  }

  @Schema(name = "quantity", required = true)
  public String getQuantity() {
    return quantity;
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
    OrderProductDTO orderProductDTO = (OrderProductDTO) o;
    return Objects.equals(this.productRef, orderProductDTO.productRef) && Objects.equals(this.price, orderProductDTO.price) && Objects.equals(this.quantity, orderProductDTO.quantity) && Objects.equals(this.ref, orderProductDTO.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(productRef, price, quantity, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderProductDTO{");
    sb.append(" productRef:").append(productRef).append(",");
    sb.append(" price:").append(price).append(",");
    sb.append(" quantity:").append(quantity).append(",");
    sb.append(" ref:").append(ref);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.productRef)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.price)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.quantity)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.ref)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderProductDTO");
    }
  }

}
