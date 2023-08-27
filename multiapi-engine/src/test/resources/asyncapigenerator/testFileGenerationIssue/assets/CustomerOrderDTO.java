package com.sngular.scsplugin.filegenerationissue.model.event.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
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
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetails;
  @JsonProperty(value ="orderedItems")
  private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO>();
  @JsonProperty(value ="paymentDetails")
  private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO>();
  @JsonProperty(value ="status")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO status;
  @JsonProperty(value ="customer")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customer;

  private CustomerOrderDTO(String id, String date, com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetails, List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems, List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails, com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO status, com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customer) {
    this.id = id;
    this.date = date;
    this.shippingDetails = shippingDetails;
    this.orderedItems = orderedItems;
    this.paymentDetails = paymentDetails;
    this.status = status;
    this.customer = customer;

  }

  private CustomerOrderDTO(CustomerOrderDTOBuilder builder) {
    this.id = builder.id;
    this.date = builder.date;
    this.shippingDetails = builder.shippingDetails;
    this.orderedItems = builder.orderedItems;
    this.paymentDetails = builder.paymentDetails;
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
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetails;
    private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO>();
    private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO>();
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO status;
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customer;

    public CustomerOrderDTO.CustomerOrderDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder date(String date) {
      this.date = date;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder shippingDetails(com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetails) {
      this.shippingDetails = shippingDetails;
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder orderedItems(List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems) {
      if (!orderedItems.isEmpty()) {
        this.orderedItems.addAll(orderedItems);
      }
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder orderedItem(com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO orderedItem) {
      if (orderedItem != null) {
        this.orderedItems.add(orderedItem);
      }
      return this;
    }
    public CustomerOrderDTO.CustomerOrderDTOBuilder paymentDetails(List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails) {
      if (!paymentDetails.isEmpty()) {
        this.paymentDetails.addAll(paymentDetails);
      }
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder paymentDetail(com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO paymentDetail) {
      if (paymentDetail != null) {
        this.paymentDetails.add(paymentDetail);
      }
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder status(com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO status) {
      this.status = status;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder customer(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customer) {
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
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO getShippingDetails() {
    return shippingDetails;
  }
  public void setShippingDetails(com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetails) {
    this.shippingDetails = shippingDetails;
  }

  /**
  * Get orderedItems
  * @return orderedItems
  */
  @Schema(name = "orderedItems", required = false)
  public List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> getOrderedItems() {
    return orderedItems;
  }
  public void setOrderedItems(List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems) {
    this.orderedItems = orderedItems;
  }

  /**
  * Get paymentDetails
  * @return paymentDetails
  */
  @Schema(name = "paymentDetails", required = false)
  public List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> getPaymentDetails() {
    return paymentDetails;
  }
  public void setPaymentDetails(List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails) {
    this.paymentDetails = paymentDetails;
  }

  /**
  * Get status
  * @return status
  */
  @Schema(name = "status", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO getStatus() {
    return status;
  }
  public void setStatus(com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO status) {
    this.status = status;
  }

  /**
  * Get customer
  * @return customer
  */
  @Schema(name = "customer", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO getCustomer() {
    return customer;
  }
  public void setCustomer(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customer) {
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
