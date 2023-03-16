package com.sngular.scsplugin.filegeneration.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = OrderProductMapper.OrderProductMapperBuilder.class)
public class OrderProductMapper {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="productRef")
  private String productRef;
  @JsonProperty(value ="price")
  private String price;
  @JsonProperty(value ="quantity")
  private String quantity;

  private OrderProductMapper(String ref, String productRef, String price, String quantity) {
    this.ref = ref;
    this.productRef = productRef;
    this.price = price;
    this.quantity = quantity;

  }

  private OrderProductMapper(OrderProductMapperBuilder builder) {
    this.ref = builder.ref;
    this.productRef = builder.productRef;
    this.price = builder.price;
    this.quantity = builder.quantity;

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
  @Schema(name = "ref", required = false)
  public String getRef() {
    return ref;
  }
  public void setRef(String ref) {
    this.ref = ref;
  }

  /**
  * Get productRef
  * @return productRef
  */
  @Schema(name = "productRef", required = false)
  public String getProductRef() {
    return productRef;
  }
  public void setProductRef(String productRef) {
    this.productRef = productRef;
  }

  /**
  * Get price
  * @return price
  */
  @Schema(name = "price", required = false)
  public String getPrice() {
    return price;
  }
  public void setPrice(String price) {
    this.price = price;
  }

  /**
  * Get quantity
  * @return quantity
  */
  @Schema(name = "quantity", required = false)
  public String getQuantity() {
    return quantity;
  }
  public void setQuantity(String quantity) {
    this.quantity = quantity;
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
    sb.append("class OrderProductMapper {\n");
    sb.append(" ref: ").append(toIndentedString(ref)).append("\n");
    sb.append(" productRef: ").append(toIndentedString(productRef)).append("\n");
    sb.append(" price: ").append(toIndentedString(price)).append("\n");
    sb.append(" quantity: ").append(toIndentedString(quantity)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }



}
