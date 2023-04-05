package com.sngular.scsplugin.filegenerationissue.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.Size;
import com.sngular.scsplugin.filegenerationissue.model.event.exception.ModelClassException;
import com.sngular.scsplugin.filegenerationissue.model.event.customvalidator.NotNull;

@JsonDeserialize(builder = OrderedItemDTO.OrderedItemDTOBuilder.class)
public class OrderedItemDTO {

  @JsonProperty(value ="catalogItemId")
  private Long catalogItemId;
  @JsonProperty(value ="name")
  @Size(min =3, max =250)
  @NotNull
  private final String name;
  @JsonProperty(value ="quantity")
  private Integer quantity;
  @JsonProperty(value ="price")
  @NotNull
  private final Double price;

  private OrderedItemDTO(Long catalogItemId, String name, Integer quantity, Double price) {
    this.catalogItemId = catalogItemId;
    this.name = name;
    this.quantity = quantity;
    this.price = price;

    validateRequiredAttributes();
  }

  private OrderedItemDTO(OrderedItemDTOBuilder builder) {
    this.catalogItemId = builder.catalogItemId;
    this.name = builder.name;
    this.quantity = builder.quantity;
    this.price = builder.price;

    validateRequiredAttributes();
  }

  public static OrderedItemDTO.OrderedItemDTOBuilder builder() {
    return new OrderedItemDTO.OrderedItemDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderedItemDTOBuilder {

    private Long catalogItemId;
    private String name;
    private Integer quantity;
    private Double price;

    public OrderedItemDTO.OrderedItemDTOBuilder catalogItemId(Long catalogItemId) {
      this.catalogItemId = catalogItemId;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder quantity(Integer quantity) {
      this.quantity = quantity;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder price(Double price) {
      this.price = price;
      return this;
    }

    public OrderedItemDTO build() {
      OrderedItemDTO orderedItemDTO = new OrderedItemDTO(this);
      return orderedItemDTO;
    }
  }

  /**
  * Get catalogItemId
  * @return catalogItemId
  */
  @Schema(name = "catalogItemId", required = false)
  public Long getCatalogItemId() {
    return catalogItemId;
  }
  public void setCatalogItemId(Long catalogItemId) {
    this.catalogItemId = catalogItemId;
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  /**
  * Get quantity
  * @return quantity
  */
  @Schema(name = "quantity", required = false)
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
    this.quantity = quantity;
  }

  /**
  * Get price
  * @return price
  */
  @Schema(name = "price", required = true)
  public Double getPrice() {
    return price;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OrderedItemDTO orderedItemDTO = (OrderedItemDTO) o;
    return Objects.equals(this.catalogItemId, orderedItemDTO.catalogItemId) && Objects.equals(this.name, orderedItemDTO.name) && Objects.equals(this.quantity, orderedItemDTO.quantity) && Objects.equals(this.price, orderedItemDTO.price);
  }

  @Override
  public int hashCode() {
    return Objects.hash(catalogItemId, name, quantity, price);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OrderedItemDTO {\n");
    sb.append(" catalogItemId: ").append(toIndentedString(catalogItemId)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" quantity: ").append(toIndentedString(quantity)).append("\n");
    sb.append(" price: ").append(toIndentedString(price)).append("\n");
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


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    }    else if (!Objects.nonNull(this.price)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderedItemDTO");
    }
  }

}
