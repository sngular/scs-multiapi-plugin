package com.sngular.multifileplugin.testreservedwords.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = Shipment.ShipmentBuilder.class)
public class Shipment {

  @JsonProperty(value ="tags")
  private List<String> tags;
  @JsonProperty(value ="packages")
  private List<Package> packages;
  @JsonProperty(value ="_new")
  private List<String> _new;
  @JsonProperty(value ="interfaces")
  private List<String> interfaces;
  @JsonProperty(value ="_final")
  private Final _final;
  public enum Final {
    DRAFT("DRAFT"),
    SENT("SENT");

    private String value;

    Final(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  private Shipment(ShipmentBuilder builder) {
    this.tags = builder.tags;
    this.packages = builder.packages;
    this._new = builder._new;
    this.interfaces = builder.interfaces;
    this._final = builder._final;

  }

  public static Shipment.ShipmentBuilder builder() {
    return new Shipment.ShipmentBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ShipmentBuilder {

    private List<String> tags = new ArrayList<String>();
    private List<Package> packages = new ArrayList<Package>();
    private List<String> _new = new ArrayList<String>();
    private List<String> interfaces = new ArrayList<String>();
    private Final _final;

    public Shipment.ShipmentBuilder tags(List<String> tags) {
      if (!tags.isEmpty()) {
        this.tags.addAll(tags);
      }
      return this;
    }

    public Shipment.ShipmentBuilder tag(String tag) {
      if (Objects.nonNull(tag)) {
        this.tags.add(tag);
      }
      return this;
    }

    public Shipment.ShipmentBuilder packages(List<Package> packages) {
      if (!packages.isEmpty()) {
        this.packages.addAll(packages);
      }
      return this;
    }

    public Shipment.ShipmentBuilder _package(Package _package) {
      if (Objects.nonNull(_package)) {
        this.packages.add(_package);
      }
      return this;
    }

    public Shipment.ShipmentBuilder _new(List<String> _new) {
      if (!_new.isEmpty()) {
        this._new.addAll(_new);
      }
      return this;
    }

    public Shipment.ShipmentBuilder __new(String __new) {
      if (Objects.nonNull(__new)) {
        this._new.add(__new);
      }
      return this;
    }

    public Shipment.ShipmentBuilder interfaces(List<String> interfaces) {
      if (!interfaces.isEmpty()) {
        this.interfaces.addAll(interfaces);
      }
      return this;
    }

    public Shipment.ShipmentBuilder _interface(String _interface) {
      if (Objects.nonNull(_interface)) {
        this.interfaces.add(_interface);
      }
      return this;
    }

    public Shipment.ShipmentBuilder _final(Final _final) {
      this._final = _final;
      return this;
    }

    public Shipment build() {
      Shipment shipment = new Shipment(this);
      return shipment;
    }
  }

  @Schema(name = "tags", required = false)
  public List<String> getTags() {
    return tags;
  }
  public void setTags(List<String> tags) {
    this.tags = tags;
  }

  @Schema(name = "packages", required = false)
  public List<Package> getPackages() {
    return packages;
  }
  public void setPackages(List<Package> packages) {
    this.packages = packages;
  }

  @Schema(name = "new", required = false)
  public List<String> getNew() {
    return _new;
  }
  public void setNew(List<String> _new) {
    this._new = _new;
  }

  @Schema(name = "interfaces", required = false)
  public List<String> getInterfaces() {
    return interfaces;
  }
  public void setInterfaces(List<String> interfaces) {
    this.interfaces = interfaces;
  }

  @Schema(name = "final", required = false)
  public Final getFinal() {
    return _final;
  }
  public void setFinal(Final _final) {
    this._final = _final;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Shipment shipment = (Shipment) o;
    return Objects.equals(this.tags, shipment.tags) && Objects.equals(this.packages, shipment.packages) && Objects.equals(this._new, shipment._new) && Objects.equals(this.interfaces, shipment.interfaces) && Objects.equals(this._final, shipment._final);
  }

  @Override
  public int hashCode() {
    return Objects.hash(tags, packages, _new, interfaces, _final);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Shipment{");
    sb.append(" tags:").append(tags).append(",");
    sb.append(" packages:").append(packages).append(",");
    sb.append(" new:").append(_new).append(",");
    sb.append(" interfaces:").append(interfaces).append(",");
    sb.append(" final:").append(_final);
    sb.append("}");
    return sb.toString();
  }


}
