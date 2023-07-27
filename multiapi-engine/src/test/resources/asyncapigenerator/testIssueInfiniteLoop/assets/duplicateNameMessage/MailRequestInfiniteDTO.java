package com.sngular.scsplugin.infiniteLoop.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder.class)
public class MailRequestInfiniteDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO payload;

  private MailRequestInfiniteDTO(com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO payload) {
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

    private com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO payload;

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder payload(com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO payload) {
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
  public com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.infiniteLoop.model.schemas.MailRequestInfiniteDTO payload) {
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
    sb.append("MailRequestInfiniteDTO{");
    sb.append(" payload:").append(payload);
    sb.append("}");
    return sb.toString();
  }




}
