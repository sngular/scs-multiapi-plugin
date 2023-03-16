package com.sngular.scsplugin.infiniteLoop.model.schemas;

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
  @JsonProperty(value ="configDTO")
  private ConfigDTO configDTO;

  private MailRequestInfiniteDTO(String sender, List<String> recipients, ConfigDTO configDTO) {
    this.sender = sender;
    this.recipients = recipients;
    this.configDTO = configDTO;

  }

  private MailRequestInfiniteDTO(MailRequestInfiniteDTOBuilder builder) {
    this.sender = builder.sender;
    this.recipients = builder.recipients;
    this.configDTO = builder.configDTO;

  }

  public static MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder builder() {
    return new MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestInfiniteDTOBuilder {

    private String sender;
    private List<String> recipients = new ArrayList<String>();
    private ConfigDTO configDTO;

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

    public MailRequestInfiniteDTO.MailRequestInfiniteDTOBuilder configDTO(ConfigDTO configDTO) {
      this.configDTO = configDTO;
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
  * Get configDTO
  * @return configDTO
  */
  @Schema(name = "configDTO", required = false)
  public ConfigDTO getConfigDTO() {
    return configDTO;
  }
  public void setConfigDTO(ConfigDTO configDTO) {
    this.configDTO = configDTO;
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
    return Objects.equals(this.sender, mailRequestInfiniteDTO.sender) && Objects.equals(this.recipients, mailRequestInfiniteDTO.recipients) && Objects.equals(this.configDTO, mailRequestInfiniteDTO.configDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, recipients, configDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MailRequestInfiniteDTO {\n");
    sb.append(" sender: ").append(toIndentedString(sender)).append("\n");
    sb.append(" recipients: ").append(toIndentedString(recipients)).append("\n");
    sb.append(" configDTO: ").append(toIndentedString(configDTO)).append("\n");
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
