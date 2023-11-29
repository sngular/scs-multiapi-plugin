package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.exception.ModelClassException;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderProductMapper.OrderProductMapperBuilder.class)
public class OrderProductMapper {

  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;
  @JsonProperty(value ="productRef")
  @NotNull
  private final String productRef;
  @JsonProperty(value ="price")
  @NotNull
  private final String price;
  @JsonProperty(value ="quantity")
  @NotNull
  private final String quantity;

  private OrderProductMapper(OrderProductMapperBuilder builder) {
    this.ref = builder.ref;
    this.productRef = builder.productRef;
    this.price = builder.price;
    this.quantity = builder.quantity;

    validateRequiredAttributes();
  }

  public static OrderProductMapper.OrderProductMapperBuilder builder() {
    return new OrderProductMapper.OrderProductMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderProductMapperBuilder {

    private String ref;

    private String productRef;

    private String price;

    private String quantity;

    public OrderProductMapper.OrderProductMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderProductMapper.OrderProductMapperBuilder productRef(String productRef) {
      this.productRef = productRef;
      return this;
    }

    public OrderProductMapper.OrderProductMapperBuilder price(String price) {
      this.price = price;
      return this;
    }

    public OrderProductMapper.OrderProductMapperBuilder quantity(String quantity) {
      this.quantity = quantity;
      return this;
    }

    public OrderProductMapper build() {
      OrderProductMapper orderProductMapper = new OrderProductMapper(this);
      return orderProductMapper;
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
  * Get productRef
  * @return productRef
  */
  @Schema(name = "productRef", required = true)
  public String getProductRef() {
    return productRef;
  }

  /**
  * Get price
  * @return price
  */
  @Schema(name = "price", required = true)
  public String getPrice() {
    return price;
  }

  /**
  * Get quantity
  * @return quantity
  */
  @Schema(name = "quantity", required = true)
  public String getQuantity() {
    return quantity;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderProductMapper orderProductMapper = (OrderProductMapper) o;
    return Objects.equals(this.ref, orderProductMapper.ref) && Objects.equals(this.productRef, orderProductMapper.productRef) && Objects.equals(this.price, orderProductMapper.price) && Objects.equals(this.quantity, orderProductMapper.quantity);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, productRef, price, quantity);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderProductMapper{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" productRef:").append(productRef).append(",");
    sb.append(" price:").append(price).append(",");
    sb.append(" quantity:").append(quantity);
    sb.append("}");
    return sb.toString();
  }


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.ref)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.productRef)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.price)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.quantity)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderProductMapper");
    }
  }
}
