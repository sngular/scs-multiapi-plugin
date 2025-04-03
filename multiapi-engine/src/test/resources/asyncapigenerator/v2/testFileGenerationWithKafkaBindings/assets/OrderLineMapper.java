package com.sngular.scsplugin.filegenerationwithkafkabindings.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.exception.ModelClassException;
import com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLineMapper.OrderLineMapperBuilder.class)
public class OrderLineMapper {

  @JsonProperty(value ="products")
  private List<OrderProductMapper> products;
  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;

  private OrderLineMapper(OrderLineMapperBuilder builder) {
    this.products = builder.products;
    this.ref = builder.ref;

    validateRequiredAttributes();
  }

  public static OrderLineMapper.OrderLineMapperBuilder builder() {
    return new OrderLineMapper.OrderLineMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineMapperBuilder {

    private List<OrderProductMapper> products = new ArrayList<OrderProductMapper>();
    private String ref;

    public OrderLineMapper.OrderLineMapperBuilder products(List<OrderProductMapper> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLineMapper.OrderLineMapperBuilder product(OrderProductMapper product) {
      if (Objects.nonNull(product)) {
        this.products.add(product);
      }
      return this;
    }

    public OrderLineMapper.OrderLineMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }

    public OrderLineMapper build() {
      OrderLineMapper orderLineMapper = new OrderLineMapper(this);
      return orderLineMapper;
    }
  }

  @Schema(name = "products", required = false)
  public List<OrderProductMapper> getProducts() {
    return products;
  }
  public void setProducts(List<OrderProductMapper> products) {
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
    OrderLineMapper orderLineMapper = (OrderLineMapper) o;
    return Objects.equals(this.products, orderLineMapper.products) && Objects.equals(this.ref, orderLineMapper.ref);
  }

  @Override
  public int hashCode() {
    return Objects.hash(products, ref);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineMapper{");
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
      throw new ModelClassException("OrderLineMapper");
    }
  }

}
