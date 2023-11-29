package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = CustomerOrderDTO.CustomerOrderDTOBuilder.class)
public class CustomerOrderDTO {

  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="date")
  private String date;
  @JsonProperty(value ="shippingDetails")
  private ShippingDetailsDTO shippingDetails;
  @JsonProperty(value ="orderedItems")
  private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();
  @JsonProperty(value ="paymentDetails")
  private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();
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
  @JsonProperty(value ="customer")
  private CustomerDTO customer;

  private CustomerOrderDTO(CustomerOrderDTOBuilder builder) {
    this.id = builder.id;
    this.date = builder.date;
    this.shippingDetails = builder.shippingDetails;
    this.orderedItems.addAll(builder.orderedItems);
    this.paymentDetails.addAll(builder.paymentDetails);
    this.status = builder.status;
    this.customer = builder.customer;

  }

  public static CustomerOrderDTO.CustomerOrderDTOBuilder builder() {
    return new CustomerOrderDTO.CustomerOrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderDTOBuilder {

    private String id;

    private String date;

    private ShippingDetailsDTO shippingDetails;

    private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();

    private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();

    private Status status;

    private CustomerDTO customer;

    public CustomerOrderDTO.CustomerOrderDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder date(String date) {
      this.date = date;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder shippingDetails(ShippingDetailsDTO shippingDetails) {
      this.shippingDetails = shippingDetails;
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

    public CustomerOrderDTO.CustomerOrderDTOBuilder status(Status status) {
      this.status = status;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder customer(CustomerDTO customer) {
      this.customer = customer;
      return this;
    }

    public CustomerOrderDTO build() {
      CustomerOrderDTO customerOrderDTO = new CustomerOrderDTO(this);
      return customerOrderDTO;
    }
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  /**
  * Get date
  * @return date
  */
  @Schema(name = "date", required = false)
  public String getDate() {
    return date;
  }
  public void setDate(String date) {
    this.date = date;
  }

  /**
  * Get shippingDetails
  * @return shippingDetails
  */
  @Schema(name = "shippingDetails", required = false)
  public ShippingDetailsDTO getShippingDetails() {
    return shippingDetails;
  }
  public void setShippingDetails(ShippingDetailsDTO shippingDetails) {
    this.shippingDetails = shippingDetails;
  }

  /**
  * Get orderedItems
  * @return orderedItems
  */
  @Schema(name = "orderedItems", required = false)
  public List<OrderedItemDTO> getOrderedItems() {
    return orderedItems;
  }
  public void setOrderedItems(List<OrderedItemDTO> orderedItems) {
    this.orderedItems = orderedItems;
  }

  /**
  * Get paymentDetails
  * @return paymentDetails
  */
  @Schema(name = "paymentDetails", required = false)
  public List<PaymentDetailsDTO> getPaymentDetails() {
    return paymentDetails;
  }
  public void setPaymentDetails(List<PaymentDetailsDTO> paymentDetails) {
    this.paymentDetails = paymentDetails;
  }

  /**
  * Get status
  * @return status
  */
  @Schema(name = "status", required = false)
  public Status getStatus() {
    return status;
  }
  public void setStatus(Status status) {
    this.status = status;
  }

  /**
  * Get customer
  * @return customer
  */
  @Schema(name = "customer", required = false)
  public CustomerDTO getCustomer() {
    return customer;
  }
  public void setCustomer(CustomerDTO customer) {
    this.customer = customer;
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
    return Objects.equals(this.id, customerOrderDTO.id) && Objects.equals(this.date, customerOrderDTO.date) && Objects.equals(this.shippingDetails, customerOrderDTO.shippingDetails) && Objects.equals(this.orderedItems, customerOrderDTO.orderedItems) && Objects.equals(this.paymentDetails, customerOrderDTO.paymentDetails) && Objects.equals(this.status, customerOrderDTO.status) && Objects.equals(this.customer, customerOrderDTO.customer);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, date, shippingDetails, orderedItems, paymentDetails, status, customer);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderDTO{");
    sb.append(" id:").append(id).append(",");
    sb.append(" date:").append(date).append(",");
    sb.append(" shippingDetails:").append(shippingDetails).append(",");
    sb.append(" orderedItems:").append(orderedItems).append(",");
    sb.append(" paymentDetails:").append(paymentDetails).append(",");
    sb.append(" status:").append(status).append(",");
    sb.append(" customer:").append(customer);
    sb.append("}");
    return sb.toString();
  }


}
