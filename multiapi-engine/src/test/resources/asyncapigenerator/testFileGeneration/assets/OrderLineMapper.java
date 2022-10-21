package net.coru.scsplugin.filegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import net.coru.scsplugin.filegeneration.model.event.OrderProductMapper;

public class OrderLineMapper {

  @JsonProperty(value ="ref")
  private String ref;
  @JsonProperty(value ="products")
  private List<OrderProductMapper> products = new ArrayList<OrderProductMapper>();

  private OrderLineMapper(String ref, List<OrderProductMapper> products) {
    this.ref = ref;
    this.products = products;

  }

  private OrderLineMapper(OrderLineMapperBuilder builder) {
    this.ref = builder.ref;
    this.products = builder.products;

  }

  public static OrderLineMapper.OrderLineMapperBuilder builder() {
    return new OrderLineMapper.OrderLineMapperBuilder();
  }

  public static class OrderLineMapperBuilder {

    private String ref;
    private List<OrderProductMapper> products = new ArrayList<OrderProductMapper>();

    public OrderLineMapper.OrderLineMapperBuilder ref(String ref) {
      this.ref = ref;
      return this;
    }
    public OrderLineMapper.OrderLineMapperBuilder products(List<OrderProductMapper> products) {
      if (!products.isEmpty()) {
        this.products.addAll(products);
      }
      return this;
    }

    public OrderLineMapper.OrderLineMapperBuilder product(OrderProductMapper product) {
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
  public List<OrderProductMapper> getProducts() {
    return products;
  }
  public void setProducts(List<OrderProductMapper> products) {
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
    sb.append("class OrderLineMapper {\n");
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
