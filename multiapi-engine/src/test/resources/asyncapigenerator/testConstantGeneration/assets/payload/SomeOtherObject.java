package com.sngular.scsplugin.constantgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = SomeOtherObject.SomeOtherObjectBuilder.class)
public class SomeOtherObject {

  @JsonProperty(value ="property2")
  private final Integer property2 = 6;
  @JsonProperty(value ="property1")
  private final Integer property1 = 3;

  private SomeOtherObject(SomeOtherObjectBuilder builder) {

  }

  public static SomeOtherObject.SomeOtherObjectBuilder builder() {
    return new SomeOtherObject.SomeOtherObjectBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SomeOtherObjectBuilder {


    public SomeOtherObject build() {
      SomeOtherObject someOtherObject = new SomeOtherObject(this);
      return someOtherObject;
    }
  }

  @Schema(name = "property2", required = false)
  public Integer getProperty2() {
    return property2;
  }

  @Schema(name = "property1", required = false)
  public Integer getProperty1() {
    return property1;
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
    return Objects.equals(this.property2, someOtherObject.property2) && Objects.equals(this.property1, someOtherObject.property1);
  }

  @Override
  public int hashCode() {
    return Objects.hash(property2, property1);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("SomeOtherObject{");
    sb.append(" property2:").append(property2).append(",");
    sb.append(" property1:").append(property1);
    sb.append("}");
    return sb.toString();
  }


}
