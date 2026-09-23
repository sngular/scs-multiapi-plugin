package com.sngular.multifileplugin.testrarecharsnames.model;

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

  @JsonProperty(value ="2fa-token")
  private String _2faToken;
  @JsonProperty(value ="delivery.status")
  private DeliveryStatus deliveryStatus;
  public enum DeliveryStatus {
    DRAFT("DRAFT"),
    SENT("SENT");

    private String value;

    DeliveryStatus(String value) {
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
  @JsonProperty(value ="client-ref")
  private String clientRef;
  @JsonProperty(value ="package-list")
  private List<String> packageList;

  private Shipment(ShipmentBuilder builder) {
    this._2faToken = builder._2faToken;
    this.deliveryStatus = builder.deliveryStatus;
    this.clientRef = builder.clientRef;
    this.packageList = builder.packageList;

  }

  public static Shipment.ShipmentBuilder builder() {
    return new Shipment.ShipmentBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ShipmentBuilder {

    private String _2faToken;
    private DeliveryStatus deliveryStatus;
    private String clientRef;
    private List<String> packageList = new ArrayList<String>();

    @JsonProperty(value ="2fa-token")
    public Shipment.ShipmentBuilder _2faToken(String _2faToken) {
      this._2faToken = _2faToken;
      return this;
    }

    @JsonProperty(value ="delivery.status")
    public Shipment.ShipmentBuilder deliveryStatus(DeliveryStatus deliveryStatus) {
      this.deliveryStatus = deliveryStatus;
      return this;
    }

    @JsonProperty(value ="client-ref")
    public Shipment.ShipmentBuilder clientRef(String clientRef) {
      this.clientRef = clientRef;
      return this;
    }

    @JsonProperty(value ="package-list")
    public Shipment.ShipmentBuilder packageList(List<String> packageList) {
      if (Objects.nonNull(packageList) && !packageList.isEmpty()) {
        this.packageList.addAll(packageList);
      }
      return this;
    }

    public Shipment.ShipmentBuilder _package(String _package) {
      if (Objects.nonNull(_package)) {
        this.packageList.add(_package);
      }
      return this;
    }

    public Shipment build() {
      Shipment shipment = new Shipment(this);
      return shipment;
    }
  }

  @Schema(name = "2fa-token", required = false)
  public String get_2faToken() {
    return _2faToken;
  }
  public void set_2faToken(String _2faToken) {
    this._2faToken = _2faToken;
  }

  @Schema(name = "delivery.status", required = false)
  public DeliveryStatus getDeliveryStatus() {
    return deliveryStatus;
  }
  public void setDeliveryStatus(DeliveryStatus deliveryStatus) {
    this.deliveryStatus = deliveryStatus;
  }

  @Schema(name = "client-ref", required = false)
  public String getClientRef() {
    return clientRef;
  }
  public void setClientRef(String clientRef) {
    this.clientRef = clientRef;
  }

  @Schema(name = "package-list", required = false)
  public List<String> getPackageList() {
    return packageList;
  }
  public void setPackageList(List<String> packageList) {
    this.packageList = packageList;
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
    return Objects.equals(this._2faToken, shipment._2faToken) && Objects.equals(this.deliveryStatus, shipment.deliveryStatus) && Objects.equals(this.clientRef, shipment.clientRef) && Objects.equals(this.packageList, shipment.packageList);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_2faToken, deliveryStatus, clientRef, packageList);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Shipment{");
    sb.append(" 2fa-token:").append(_2faToken).append(",");
    sb.append(" delivery.status:").append(deliveryStatus).append(",");
    sb.append(" client-ref:").append(clientRef).append(",");
    sb.append(" package-list:").append(packageList);
    sb.append("}");
    return sb.toString();
  }


}
