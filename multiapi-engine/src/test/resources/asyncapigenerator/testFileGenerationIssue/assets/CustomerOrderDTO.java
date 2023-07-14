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
  @JsonProperty(value ="shippingDetailsDTO")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetailsDTO;
  @JsonProperty(value ="orderedItems")
  private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO>();
  @JsonProperty(value ="paymentDetails")
  private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO>();
  @JsonProperty(value ="orderStatusDTO")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO orderStatusDTO;
  @JsonProperty(value ="customerDTO")
  private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customerDTO;

  private CustomerOrderDTO(String id, String date, com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetailsDTO, List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems, List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails, com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO orderStatusDTO, com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customerDTO) {
    this.id = id;
    this.date = date;
    this.shippingDetailsDTO = shippingDetailsDTO;
    this.orderedItems = orderedItems;
    this.paymentDetails = paymentDetails;
    this.orderStatusDTO = orderStatusDTO;
    this.customerDTO = customerDTO;

  }

  private CustomerOrderDTO(CustomerOrderDTOBuilder builder) {
    this.id = builder.id;
    this.date = builder.date;
    this.shippingDetailsDTO = builder.shippingDetailsDTO;
    this.orderedItems = builder.orderedItems;
    this.paymentDetails = builder.paymentDetails;
    this.orderStatusDTO = builder.orderStatusDTO;
    this.customerDTO = builder.customerDTO;

  }

  public static CustomerOrderDTO.CustomerOrderDTOBuilder builder() {
    return new CustomerOrderDTO.CustomerOrderDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class CustomerOrderDTOBuilder {

    private String id;
    private String date;
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetailsDTO;
    private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO> orderedItems = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderedItemDTO>();
    private List<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO> paymentDetails = new ArrayList<com.sngular.scsplugin.filegenerationissue.model.event.schemas.PaymentDetailsDTO>();
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO orderStatusDTO;
    private com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customerDTO;

    public CustomerOrderDTO.CustomerOrderDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder date(String date) {
      this.date = date;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder shippingDetailsDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetailsDTO) {
      this.shippingDetailsDTO = shippingDetailsDTO;
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

    public CustomerOrderDTO.CustomerOrderDTOBuilder orderStatusDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO orderStatusDTO) {
      this.orderStatusDTO = orderStatusDTO;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder customerDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customerDTO) {
      this.customerDTO = customerDTO;
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
  * Get shippingDetailsDTO
  * @return shippingDetailsDTO
  */
  @Schema(name = "shippingDetailsDTO", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO getShippingDetailsDTO() {
    return shippingDetailsDTO;
  }
  public void setShippingDetailsDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.ShippingDetailsDTO shippingDetailsDTO) {
    this.shippingDetailsDTO = shippingDetailsDTO;
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
  * Get orderStatusDTO
  * @return orderStatusDTO
  */
  @Schema(name = "orderStatusDTO", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO getOrderStatusDTO() {
    return orderStatusDTO;
  }
  public void setOrderStatusDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.OrderStatusDTO orderStatusDTO) {
    this.orderStatusDTO = orderStatusDTO;
  }

  /**
  * Get customerDTO
  * @return customerDTO
  */
  @Schema(name = "customerDTO", required = false)
  public com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO getCustomerDTO() {
    return customerDTO;
  }
  public void setCustomerDTO(com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerDTO customerDTO) {
    this.customerDTO = customerDTO;
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
    return Objects.equals(this.id, customerOrderDTO.id) && Objects.equals(this.date, customerOrderDTO.date) && Objects.equals(this.shippingDetailsDTO, customerOrderDTO.shippingDetailsDTO) && Objects.equals(this.orderedItems, customerOrderDTO.orderedItems) && Objects.equals(this.paymentDetails, customerOrderDTO.paymentDetails) && Objects.equals(this.orderStatusDTO, customerOrderDTO.orderStatusDTO) && Objects.equals(this.customerDTO, customerOrderDTO.customerDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, date, shippingDetailsDTO, orderedItems, paymentDetails, orderStatusDTO, customerDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("CustomerOrderDTO{");
    sb.append(" id:").append(id).append(",");
    sb.append(" date:").append(date).append(",");
    sb.append(" shippingDetailsDTO:").append(shippingDetailsDTO).append(",");
    sb.append(" orderedItems:").append(orderedItems).append(",");
    sb.append(" paymentDetails:").append(paymentDetails).append(",");
    sb.append(" orderStatusDTO:").append(orderStatusDTO).append(",");
    sb.append(" customerDTO:").append(customerDTO).append(",");
    sb.append("}");
    return sb.toString();
  }




}
