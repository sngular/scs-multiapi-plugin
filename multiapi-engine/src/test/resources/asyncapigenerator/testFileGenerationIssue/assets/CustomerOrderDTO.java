package net.coru.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.filegenerationissue.model.event.OrderedItemDTO;
import net.coru.scsplugin.filegenerationissue.model.event.PaymentDetailsDTO;
import java.util.List;
import java.util.ArrayList;

public class CustomerOrderDTO {

  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="date")
  private String date;
  @JsonProperty(value ="shippingDetailsDTO")
  private ShippingDetailsDTO shippingDetailsDTO;
  @JsonProperty(value ="orderedItems")
  private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();
  @JsonProperty(value ="paymentDetails")
  private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();
  @JsonProperty(value ="orderStatusDTO")
  private OrderStatusDTO orderStatusDTO;
  @JsonProperty(value ="customerDTO")
  private CustomerDTO customerDTO;

  private CustomerOrderDTO(String id, String date, ShippingDetailsDTO shippingDetailsDTO, List<OrderedItemDTO> orderedItems, List<PaymentDetailsDTO> paymentDetails, OrderStatusDTO orderStatusDTO, CustomerDTO customerDTO) {
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

  public static class CustomerOrderDTOBuilder {

    private String id;
    private String date;
    private ShippingDetailsDTO shippingDetailsDTO;
    private List<OrderedItemDTO> orderedItems = new ArrayList<OrderedItemDTO>();
    private List<PaymentDetailsDTO> paymentDetails = new ArrayList<PaymentDetailsDTO>();
    private OrderStatusDTO orderStatusDTO;
    private CustomerDTO customerDTO;

    public CustomerOrderDTO.CustomerOrderDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder date(String date) {
      this.date = date;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder shippingDetailsDTO(ShippingDetailsDTO shippingDetailsDTO) {
      this.shippingDetailsDTO = shippingDetailsDTO;
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

    public CustomerOrderDTO.CustomerOrderDTOBuilder orderStatusDTO(OrderStatusDTO orderStatusDTO) {
      this.orderStatusDTO = orderStatusDTO;
      return this;
    }

    public CustomerOrderDTO.CustomerOrderDTOBuilder customerDTO(CustomerDTO customerDTO) {
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
  public ShippingDetailsDTO getShippingDetailsDTO() {
    return shippingDetailsDTO;
  }
  public void setShippingDetailsDTO(ShippingDetailsDTO shippingDetailsDTO) {
    this.shippingDetailsDTO = shippingDetailsDTO;
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
  * Get orderStatusDTO
  * @return orderStatusDTO
  */
  @Schema(name = "orderStatusDTO", required = false)
  public OrderStatusDTO getOrderStatusDTO() {
    return orderStatusDTO;
  }
  public void setOrderStatusDTO(OrderStatusDTO orderStatusDTO) {
    this.orderStatusDTO = orderStatusDTO;
  }

  /**
  * Get customerDTO
  * @return customerDTO
  */
  @Schema(name = "customerDTO", required = false)
  public CustomerDTO getCustomerDTO() {
    return customerDTO;
  }
  public void setCustomerDTO(CustomerDTO customerDTO) {
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
    sb.append("class CustomerOrderDTO {\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" date: ").append(toIndentedString(date)).append("\n");
    sb.append(" shippingDetailsDTO: ").append(toIndentedString(shippingDetailsDTO)).append("\n");
    sb.append(" orderedItems: ").append(toIndentedString(orderedItems)).append("\n");
    sb.append(" paymentDetails: ").append(toIndentedString(paymentDetails)).append("\n");
    sb.append(" orderStatusDTO: ").append(toIndentedString(orderStatusDTO)).append("\n");
    sb.append(" customerDTO: ").append(toIndentedString(customerDTO)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }



}
