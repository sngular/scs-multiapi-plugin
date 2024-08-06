package com.sngular.scsplugin.nestedobject.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = SomeOtherObject.SomeOtherObjectBuilder.class)
public class SomeOtherObject {

  @JsonProperty(value ="property2")
  private Integer property2;
  @JsonProperty(value ="property1")
  private String property1;

  private SomeOtherObject(SomeOtherObjectBuilder builder) {
    this.property2 = builder.property2;
    this.property1 = builder.property1;

  }

  public static SomeOtherObject.SomeOtherObjectBuilder builder() {
    return new SomeOtherObject.SomeOtherObjectBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class SomeOtherObjectBuilder {

    private Integer property2;
    private String property1;

    public SomeOtherObject.SomeOtherObjectBuilder property2(Integer property2) {
      this.property2 = property2;
      return this;
    }

    public SomeOtherObject.SomeOtherObjectBuilder property1(String property1) {
      this.property1 = property1;
      return this;
    }

    public SomeOtherObject build() {
      SomeOtherObject someOtherObject = new SomeOtherObject(this);
      return someOtherObject;
    }
  }

  @Schema(name = "property2", required = false)
  public Integer getProperty2() {
    return property2;
  }
  public void setProperty2(Integer property2) {
    this.property2 = property2;
  }

  @Schema(name = "property1", required = false)
  public String getProperty1() {
    return property1;
  }
  public void setProperty1(String property1) {
    this.property1 = property1;
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
