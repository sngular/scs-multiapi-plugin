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
  @JsonProperty(value ="recipients")
  private List<String> recipients = new ArrayList<String>();
  @JsonProperty(value ="config")
  private ConfigDTO config;

  private MailRequestInfiniteDTO(String sender, List<String> recipients, ConfigDTO config) {
    this.sender = sender;
    this.recipients = recipients;
    this.config = config;

  }

  private MailRequestInfiniteDTO(MailRequestInfiniteDTOBuilder builder) {
    this.sender = builder.sender;
    this.recipients = builder.recipients;
    this.config = builder.config;

  }

  public static MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder builder() {
    return new MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestInfiniteDTOBuilder {

    private String sender;
    private List<String> recipients = new ArrayList<String>();
    private ConfigDTO config;

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder sender(String sender) {
      this.sender = sender;
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

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder config(ConfigDTO config) {
      this.config = config;
      return this;
    }

    public MailRequestInfiniteDTO build() {
      MailRequestInfiniteDTO mailRequestInfiniteDTO = new MailRequestInfiniteDTO(this);
      return mailRequestInfiniteDTO;
    }
  }

  /**
  * Get sender
  * @return sender
  */
  @Schema(name = "sender", required = false)
  public String getSender() {
    return sender;
  }
  public void setSender(String sender) {
    this.sender = sender;
  }

  /**
  * Get recipients
  * @return recipients
  */
  @Schema(name = "recipients", required = false)
  public List<String> getRecipients() {
    return recipients;
  }
  public void setRecipients(List<String> recipients) {
    this.recipients = recipients;
  }

  /**
  * Get config
  * @return config
  */
  @Schema(name = "config", required = false)
  public ConfigDTO getConfig() {
    return config;
  }
  public void setConfig(ConfigDTO config) {
    this.config = config;
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
    return Objects.equals(this.sender, mailRequestInfiniteDTO.sender) && Objects.equals(this.recipients, mailRequestInfiniteDTO.recipients) && Objects.equals(this.config, mailRequestInfiniteDTO.config);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, recipients, config);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MailRequestInfiniteDTO{");
    sb.append(" sender:").append(sender).append(",");
    sb.append(" recipients:").append(recipients).append(",");
    sb.append(" config:").append(config);
    sb.append("}");
    return sb.toString();
  }




}
