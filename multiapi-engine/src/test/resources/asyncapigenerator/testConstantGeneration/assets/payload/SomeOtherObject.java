package com.sngular.scsplugin.constantgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = SomeOtherObject.SomeOtherObjectBuilder.class)
public class SomeOtherObject {

  @JsonProperty(value ="property1")
  private final Integer property1;
  @JsonProperty(value ="property2")
  private final Integer property2;

  private SomeOtherObject(SomeOtherObjectBuilder builder) {
    this.property1 = builder.property1;
    this.property2 = builder.property2;

  }

  public static SomeOtherObject.SomeOtherObjectBuilder builder() {
    return new SomeOtherObject.SomeOtherObjectBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SomeOtherObjectBuilder {

    private Integer property1 = 3;

    private Integer property2 = 6;

    public SomeOtherObject build() {
      SomeOtherObject someOtherObject = new SomeOtherObject(this);
      return someOtherObject;
    }
  }

  /**
  * Get property1
  * @return property1
  */
  @Schema(name = "property1", required = false)
  public Integer getProperty1() {
    return property1;
  }

  /**
  * Get property2
  * @return property2
  */
  @Schema(name = "property2", required = false)
  public Integer getProperty2() {
    return property2;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SomeOtherObject someOtherObject = (SomeOtherObject) o;
    return Objects.equals(this.property1, someOtherObject.property1) && Objects.equals(this.property2, someOtherObject.property2);
  }

  @Override
  public int hashCode() {
    return Objects.hash(property1, property2);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SomeOtherObject{");
    sb.append(" property1:").append(property1).append(",");
    sb.append(" property2:").append(property2);
    sb.append("}");
    return sb.toString();
  }


}