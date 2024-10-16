package com.sngular.scsplugin.filegenerationissue.model.event;

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
  @JsonProperty(value ="price")
  @NotNull
  private final Double price;
  @JsonProperty(value ="quantity")
  private Integer quantity;

  private OrderedItemDTO(OrderedItemDTOBuilder builder) {
    this.catalogItemId = builder.catalogItemId;
    this.name = builder.name;
    this.price = builder.price;
    this.quantity = builder.quantity;

    validateRequiredAttributes();
  }

  public static OrderedItemDTO.OrderedItemDTOBuilder builder() {
    return new OrderedItemDTO.OrderedItemDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OrderedItemDTOBuilder {

    private Long catalogItemId;
    private String name;
    private Double price;
    private Integer quantity;

    public OrderedItemDTO.OrderedItemDTOBuilder catalogItemId(Long catalogItemId) {
      this.catalogItemId = catalogItemId;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder price(Double price) {
      this.price = price;
      return this;
    }

    public OrderedItemDTO.OrderedItemDTOBuilder quantity(Integer quantity) {
      this.quantity = quantity;
      return this;
    }

    public OrderedItemDTO build() {
      OrderedItemDTO orderedItemDTO = new OrderedItemDTO(this);
      return orderedItemDTO;
    }
  }

  @Schema(name = "catalogItemId", required = false)
  public Long getCatalogItemId() {
    return catalogItemId;
  }
  public void setCatalogItemId(Long catalogItemId) {
    this.catalogItemId = catalogItemId;
  }

  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  @Schema(name = "price", required = true)
  public Double getPrice() {
    return price;
  }

  @Schema(name = "quantity", required = false)
  public Integer getQuantity() {
    return quantity;
  }
  public void setQuantity(Integer quantity) {
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
    OrderedItemDTO orderedItemDTO = (OrderedItemDTO) o;
    return Objects.equals(this.catalogItemId, orderedItemDTO.catalogItemId) && Objects.equals(this.name, orderedItemDTO.name) && Objects.equals(this.price, orderedItemDTO.price) && Objects.equals(this.quantity, orderedItemDTO.quantity);
  }

  @Override
  public int hashCode() {
    return Objects.hash(catalogItemId, name, price, quantity);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("OrderedItemDTO{");
    sb.append(" catalogItemId:").append(catalogItemId).append(",");
    sb.append(" name:").append(name).append(",");
    sb.append(" price:").append(price).append(",");
    sb.append(" quantity:").append(quantity);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.price)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("OrderedItemDTO");
    }
  }

}
