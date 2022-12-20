package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class PaymentDetailsDTO {

  @JsonProperty(value ="creditCardNumber")
  private String creditCardNumber;

  private PaymentDetailsDTO(String creditCardNumber) {
    this.creditCardNumber = creditCardNumber;

  }

  private PaymentDetailsDTO(PaymentDetailsDTOBuilder builder) {
    this.creditCardNumber = builder.creditCardNumber;

  }

  public static PaymentDetailsDTO.PaymentDetailsDTOBuilder builder() {
    return new PaymentDetailsDTO.PaymentDetailsDTOBuilder();
  }

  public static class PaymentDetailsDTOBuilder {

    private String creditCardNumber;

    public PaymentDetailsDTO.PaymentDetailsDTOBuilder creditCardNumber(String creditCardNumber) {
      this.creditCardNumber = creditCardNumber;
      return this;
    }

    public PaymentDetailsDTO build() {
      PaymentDetailsDTO paymentDetailsDTO = new PaymentDetailsDTO(this);
      return paymentDetailsDTO;
    }
  }

  /**
  * Get creditCardNumber
  * @return creditCardNumber
  */
  @Schema(name = "creditCardNumber", required = false)
  public String getCreditCardNumber() {
    return creditCardNumber;
  }
  public void setCreditCardNumber(String creditCardNumber) {
    this.creditCardNumber = creditCardNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PaymentDetailsDTO paymentDetailsDTO = (PaymentDetailsDTO) o;
    return Objects.equals(this.creditCardNumber, paymentDetailsDTO.creditCardNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(creditCardNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PaymentDetailsDTO {\n");
    sb.append(" creditCardNumber: ").append(toIndentedString(creditCardNumber)).append("\n");
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
