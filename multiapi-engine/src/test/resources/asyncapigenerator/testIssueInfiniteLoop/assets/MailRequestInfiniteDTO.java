package com.sngular.scsplugin.infiniteLoop.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder.class)
public class MailRequestInfiniteDTO {

  @JsonProperty(value ="sender")
  private String sender;
  @JsonProperty(value ="config")
  private ConfigDTO config;
  @JsonProperty(value ="recipients")
  private List<String> recipients;

  private MailRequestInfiniteDTO(MailRequestInfiniteDTOBuilder builder) {
    this.sender = builder.sender;
    this.config = builder.config;
    this.recipients = builder.recipients;

  }

  public static MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder builder() {
    return new MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestInfiniteDTOBuilder {

    private String sender;
    private ConfigDTO config;
    private List<String> recipients = new ArrayList<String>();

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder sender(String sender) {
      this.sender = sender;
      return this;
    }

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder config(ConfigDTO config) {
      this.config = config;
      return this;
    }

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder recipients(List<String> recipients) {
      if (!recipients.isEmpty()) {
        this.recipients.addAll(recipients);
      }
      return this;
    }

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder recipient(String recipient) {
      if (recipient != null) {
        this.recipients.add(recipient);
      }
      return this;
    }

    public MailRequestInfiniteDTO build() {
      MailRequestInfiniteDTO mailRequestInfiniteDTO = new MailRequestInfiniteDTO(this);
      return mailRequestInfiniteDTO;
    }
  }

  @Schema(name = "sender", required = false)
  public String getSender() {
    return sender;
  }
  public void setSender(String sender) {
    this.sender = sender;
  }

  @Schema(name = "config", required = false)
  public ConfigDTO getConfig() {
    return config;
  }
  public void setConfig(ConfigDTO config) {
    this.config = config;
  }

  @Schema(name = "recipients", required = false)
  public List<String> getRecipients() {
    return recipients;
  }
  public void setRecipients(List<String> recipients) {
    this.recipients = recipients;
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
    return Objects.equals(this.sender, mailRequestInfiniteDTO.sender) && Objects.equals(this.config, mailRequestInfiniteDTO.config) && Objects.equals(this.recipients, mailRequestInfiniteDTO.recipients);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, config, recipients);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MailRequestInfiniteDTO{");
    sb.append(" sender:").append(sender).append(",");
    sb.append(" config:").append(config).append(",");
    sb.append(" recipients:").append(recipients);
    sb.append("}");
    return sb.toString();
  }


}
