package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import java.time.LocalDateTime;

@JsonDeserialize(builder = CustomerOrderDTO.CustomerOrderDTOBuilder.class)
public class CustomerOrderDTO {

  @JsonProperty(value ="status")
  private Status status;
  public enum Status {
    DELIVERED("DELIVERED"),
    CONFIRMED("CONFIRMED"),
    SHIPPED("SHIPPED");

    private String value;

    Status(String value) {
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
  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="date")
  private LocalDateTime date;
  @JsonProperty(value ="orderedItems")
  private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();
  @JsonProperty(value ="paymentDetails")
  private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();
  @JsonProperty(value ="customer")
  private CustomerDTO customer;
  @JsonProperty(value ="shippingDetails")
  private ShippingDetailsDTO shippingDetails;

  private CustomerOrderDTO(CustomerOrderDTOBuilder builder) {
    this.status = builder.status;
    this.id = builder.id;
    this.date = builder.date;
    this.orderedItems = builder.orderedItems;
    this.paymentDetails = builder.paymentDetails;
    this.customer = builder.customer;
    this.shippingDetails = builder.shippingDetails;

  }

  public static CustomerOrderDTO.CustomerOrderDTOBuilder builder() {
    return new CustomerOrderDTO.CustomerOrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderDTOBuilder {

    private Status status;
    private String id;
    private LocalDateTime date;
    private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();
    private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();
    private CustomerDTO customer;
    private ShippingDetailsDTO shippingDetails;
    public CustomerOrderDTO.CustomerOrderDTOBuilder status(Status status) {
      this.status = status;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder date(LocalDateTime date) {
      this.date = date;
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder orderedItems(List<OrderedItemDTO> orderedItems) {
      if (!orderedItems.isEmpty()) {
        this.orderedItems.addAll(orderedItems);
      }
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder orderedItem(OrderedItemDTO orderedItem) {
      if (orderedItem != null) {
        this.orderedItems.add(orderedItem);
      }
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder paymentDetails(List<PaymentDetailsDTO> paymentDetails) {
      if (!paymentDetails.isEmpty()) {
        this.paymentDetails.addAll(paymentDetails);
      }
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder paymentDetail(PaymentDetailsDTO paymentDetail) {
      if (paymentDetail != null) {
        this.paymentDetails.add(paymentDetail);
      }
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder customer(CustomerDTO customer) {
      this.customer = customer;
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder shippingDetails(ShippingDetailsDTO shippingDetails) {
      this.shippingDetails = shippingDetails;
      return this;
    }

    public CustomerOrderDTO build() {
      CustomerOrderDTO customerOrderDTO = new CustomerOrderDTO(this);
      return customerOrderDTO;
    }
  }

  @Schema(name = "status", required = false)
  public Status getStatus() {
    return status;
  }
  public void setStatus(Status status) {
    this.status = status;
  }

  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Schema(name = "date", required = false)
  public LocalDateTime getDate() {
    return date;
  }
  public void setDate(LocalDateTime date) {
    this.date = date;
  }

  @Schema(name = "orderedItems", required = false)
  public List<OrderedItemDTO> getOrderedItems() {
    return orderedItems;
  }
  public void setOrderedItems(List<OrderedItemDTO> orderedItems) {
    this.orderedItems = orderedItems;
  }

  @Schema(name = "paymentDetails", required = false)
  public List<PaymentDetailsDTO> getPaymentDetails() {
    return paymentDetails;
  }
  public void setPaymentDetails(List<PaymentDetailsDTO> paymentDetails) {
    this.paymentDetails = paymentDetails;
  }

  @Schema(name = "customer", required = false)
  public CustomerDTO getCustomer() {
    return customer;
  }
  public void setCustomer(CustomerDTO customer) {
    this.customer = customer;
  }

  @Schema(name = "shippingDetails", required = false)
  public ShippingDetailsDTO getShippingDetails() {
    return shippingDetails;
  }
  public void setShippingDetails(ShippingDetailsDTO shippingDetails) {
    this.shippingDetails = shippingDetails;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CustomerOrderDTO customerOrderDTO = (CustomerOrderDTO) o;
    return Objects.equals(this.status, customerOrderDTO.status) && Objects.equals(this.id, customerOrderDTO.id) && Objects.equals(this.date, customerOrderDTO.date) && Objects.equals(this.orderedItems, customerOrderDTO.orderedItems) && Objects.equals(this.paymentDetails, customerOrderDTO.paymentDetails) && Objects.equals(this.customer, customerOrderDTO.customer) && Objects.equals(this.shippingDetails, customerOrderDTO.shippingDetails);
  }

  @Override
  public int hashCode() {
    return Objects.hash(status, id, date, orderedItems, paymentDetails, customer, shippingDetails);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderDTO{");
    sb.append(" status:").append(status).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append(" date:").append(date).append(",");
    sb.append(" orderedItems:").append(orderedItems).append(",");
    sb.append(" paymentDetails:").append(paymentDetails).append(",");
    sb.append(" customer:").append(customer).append(",");
    sb.append(" shippingDetails:").append(shippingDetails);
    sb.append("}");
    return sb.toString();
  }


}
