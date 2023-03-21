package com.sngular.scsplugin.filegeneration.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = OrderLineDTO.OrderLineDTOBuilder.class)
public class OrderLineDTO {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="products")
  private List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> products = new ArrayList<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO>();

  private OrderLineDTO(String ref, List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> products) {
    this.ref = ref;
    this.products = products;

  }

  private OrderLineDTO(OrderLineDTOBuilder builder) {
    this.ref = builder.ref;
    this.products = builder.products;

  }

  public static OrderLineDTO.OrderLineDTOBuilder builder() {
    return new OrderLineDTO.OrderLineDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderLineDTOBuilder {

    private String ref;
    private List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> products = new ArrayList<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO>();

    public OrderLineDTO.OrderLineDTOBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }
    public OrderLineDTO.OrderLineDTOBuilder products(List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLineDTO.OrderLineDTOBuilder product(com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO product) {
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
  @Schema(name = "ref", required = false)
  public String getRef() {
    return ref;
  }
  public void setRef(String ref) {
    this.ref = ref;
  }

  /**
  * Get products
  * @return products
  */
  @Schema(name = "products", required = false)
  public List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> getProducts() {
    return products;
  }
  public void setProducts(List<com.sngular.scsplugin.filegeneration.model.event.schemas.OrderProductDTO> products) {
    this.products = products;
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
    sb.append("class OrderLineDTO {\n");
    sb.append(" ref: ").append(toIndentedString(ref)).append("\n");
    sb.append(" products: ").append(toIndentedString(products)).append("\n");
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
