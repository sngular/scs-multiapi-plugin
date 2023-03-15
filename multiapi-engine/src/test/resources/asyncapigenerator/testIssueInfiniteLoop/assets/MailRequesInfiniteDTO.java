package com.sngular.scsplugin.infiniteLoop.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.infiniteLoop.model.MailRequestInfiniteDTO;

@JsonDeserialize(builder = MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder.class)
public class MailRequestInfiniteDTO {

  @JsonProperty(value ="payload")
  private MailRequestInfiniteDTO payload;

  private MailRequestInfiniteDTO(MailRequestInfiniteDTO payload) {
    this.payload = payload;

  }

  private MailRequestInfiniteDTO(MailRequestInfiniteDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder builder() {
    return new MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestInfiniteDTOBuilder {

    private MailRequestInfiniteDTO payload;

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder payload(MailRequestInfiniteDTO payload) {
      this.payload = payload;
      return this;
    }

    public MailRequestInfiniteDTO build() {
      MailRequestInfiniteDTO mailRequestInfiniteDTO = new MailRequestInfiniteDTO(this);
      return mailRequestInfiniteDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public MailRequestInfiniteDTO getPayload() {
    return payload;
  }
  public void setPayload(MailRequestInfiniteDTO payload) {
    this.payload = payload;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MailRequestInfiniteDTO mailRequestInfiniteDTO = (MailRequestInfiniteDTO) o;
    return Objects.equals(this.payload, mailRequestInfiniteDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MailRequestInfiniteDTO {\n");
    sb.append(" payload: ").append(toIndentedString(payload)).append("\n");
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
