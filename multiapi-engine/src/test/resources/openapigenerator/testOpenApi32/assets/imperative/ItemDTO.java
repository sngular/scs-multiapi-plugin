package com.sngular.multifileplugin.openapi32.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ItemDTO.ItemDTOBuilder.class)
public class ItemDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="id")
  private String id;

  private ItemDTO(ItemDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

  }

  public static ItemDTO.ItemDTOBuilder builder() {
    return new ItemDTO.ItemDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ItemDTOBuilder {

    private String name;
    private String id;

    public ItemDTO.ItemDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ItemDTO.ItemDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ItemDTO build() {
      ItemDTO itemDTO = new ItemDTO(this);
      return itemDTO;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ItemDTO itemDTO = (ItemDTO) o;
    return Objects.equals(this.name, itemDTO.name) && Objects.equals(this.id, itemDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ItemDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
