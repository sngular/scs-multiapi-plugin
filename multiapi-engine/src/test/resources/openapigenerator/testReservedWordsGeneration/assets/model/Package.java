package com.sngular.multifileplugin.testreservedwords.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Package.PackageBuilder.class)
public class Package {

  @JsonProperty(value ="weight")
  private Double weight;
  @JsonProperty(value ="id")
  private String id;

  private Package(PackageBuilder builder) {
    this.weight = builder.weight;
    this.id = builder.id;

  }

  public static Package.PackageBuilder builder() {
    return new Package.PackageBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class PackageBuilder {

    private Double weight;
    private String id;

    public Package.PackageBuilder weight(Double weight) {
      this.weight = weight;
      return this;
    }

    public Package.PackageBuilder id(String id) {
      this.id = id;
      return this;
    }

    public Package build() {
      Package _package = new Package(this);
      return _package;
    }
  }

  @Schema(name = "weight", required = false)
  public Double getWeight() {
    return weight;
  }
  public void setWeight(Double weight) {
    this.weight = weight;
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
    Package _package = (Package) o;
    return Objects.equals(this.weight, _package.weight) && Objects.equals(this.id, _package.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(weight, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Package{");
    sb.append(" weight:").append(weight).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
  }


}
