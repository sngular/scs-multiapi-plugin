package com.sngular.scsplugin.noschemas.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Thing.ThingBuilder.class)
public class Thing {

  @JsonProperty(value ="id")
  private Integer id;

  private Thing(Integer id) {
    this.id = id;

  }

  private Thing(ThingBuilder builder) {
    this.id = builder.id;

  }

  public static Thing.ThingBuilder builder() {
    return new Thing.ThingBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ThingBuilder {

    private Integer id;

    public Thing.ThingBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public Thing build() {
      Thing thing = new Thing(this);
      return thing;
    }
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
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
    Thing thing = (Thing) o;
    return Objects.equals(this.id, thing.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Thing{");
    sb.append(" id:").append(toIndentedString(id)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
