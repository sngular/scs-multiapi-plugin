package com.sngular.apigenerator.asyncapi.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.apigenerator.asyncapi.model.schemas.exception.ModelClassException;
import com.sngular.apigenerator.asyncapi.model.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLine.OrderLineBuilder.class)
public class OrderLine {

  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;
  @JsonProperty(value ="products")
  @NotNull
  private final List<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct> products;

  private OrderLine(String ref, List<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct> products) {
    this.ref = ref;
    this.products = products;

    validateRequiredAttributes();
  }

  private OrderLine(OrderLineBuilder builder) {
    this.ref = builder.ref;
    this.products = builder.products;

    validateRequiredAttributes();
  }

  public static OrderLine.OrderLineBuilder builder() {
    return new OrderLine.OrderLineBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineBuilder {

    private String ref;
    private List<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct> products = new ArrayList<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct>();

    public OrderLine.OrderLineBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }
    public OrderLine.OrderLineBuilder products(List<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLine.OrderLineBuilder product(com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct product) {
      if (product != null) {
        this.products.add(product);
      }
      return this;
    }

    public OrderLine build() {
      OrderLine orderLine = new OrderLine(this);
      return orderLine;
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
  public List<com.sngular.apigenerator.asyncapi.model.schemas.OrderProduct> getProducts() {
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
    OrderLine orderLine = (OrderLine) o;
    return Objects.equals(this.ref, orderLine.ref) && Objects.equals(this.products, orderLine.products);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, products);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLine{");
    sb.append(" ref:").append(ref).append(",");
    sb.append(" products:").append(products).append(",");
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
      throw new ModelClassException("OrderLine");
    }
  }

}
