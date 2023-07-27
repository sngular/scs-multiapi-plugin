package com.sngular.scsplugin.filegeneration.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.scsplugin.filegeneration.model.event.schemas.exception.ModelClassException;
import com.sngular.scsplugin.filegeneration.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderLineMapper.OrderLineMapperBuilder.class)
public class OrderLineMapper {

  @JsonProperty(value ="ref")
  @NotNull
  private final String ref;
  @JsonProperty(value ="products")
  @NotNull
  private final List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper> products;

  private OrderLineMapper(String ref, List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper> products) {
    this.ref = ref;
    this.products = products;

    validateRequiredAttributes();
  }

  private OrderLineMapper(OrderLineMapperBuilder builder) {
    this.ref = builder.ref;
    this.products = builder.products;

    validateRequiredAttributes();
  }

  public static OrderLineMapper.OrderLineMapperBuilder builder() {
    return new OrderLineMapper.OrderLineMapperBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineMapperBuilder {

    private String ref;
    private List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper> products = new ArrayList<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper>();

    public OrderLineMapper.OrderLineMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }
    public OrderLineMapper.OrderLineMapperBuilder products(List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLineMapper.OrderLineMapperBuilder product(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper product) {
      if (product != null) {
        this.products.add(product);
      }
      return this;
    }

    public OrderLineMapper build() {
      OrderLineMapper orderLineMapper = new OrderLineMapper(this);
      return orderLineMapper;
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
  public List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductMapper> getProducts() {
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
    OrderLineMapper orderLineMapper = (OrderLineMapper) o;
    return Objects.equals(this.ref, orderLineMapper.ref) && Objects.equals(this.products, orderLineMapper.products);
  }

  @Override
  public int hashCode() {
    return Objects.hash(ref, products);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderLineMapper{");
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
      throw new ModelClassException("OrderLineMapper");
    }
  }

}
