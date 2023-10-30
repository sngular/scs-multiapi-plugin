package com.sngular.scsplugin.filegenerationissue.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = PaymentDetailsDTO.PaymentDetailsDTOBuilder.class)
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

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
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
    sb.append("PaymentDetailsDTO{");
    sb.append(" creditCardNumber:").append(creditCardNumber);
    sb.append("}");
    return sb.toString();
  }


}
