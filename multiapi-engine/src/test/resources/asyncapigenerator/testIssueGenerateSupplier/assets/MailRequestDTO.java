package company.mail.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;

@JsonDeserialize(builder = MailRequestDTO.MailRequestDTOBuilder.class)
public class MailRequestDTO {

  @JsonProperty(value ="sender")
  private String sender;
  @JsonProperty(value ="recipients")
  private List<String> recipients = new ArrayList<String>();
  @JsonProperty(value ="config")
  private ConfigDTO config;

  private MailRequestDTO(String sender, List<String> recipients, ConfigDTO config) {
    this.sender = sender;
    this.recipients = recipients;
    this.config = config;

  }

  private MailRequestDTO(MailRequestDTOBuilder builder) {
    this.sender = builder.sender;
    this.recipients = builder.recipients;
    this.config = builder.config;

  }

  public static MailRequestDTO.MailRequestDTOBuilder builder() {
    return new MailRequestDTO.MailRequestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestDTOBuilder {

    private String sender;
    private List<String> recipients = new ArrayList<String>();
    private ConfigDTO config;

    public MailRequestDTO.MailRequestDTOBuilder sender(String sender) {
      this.sender = sender;
      return this;
    }

    public MailRequestDTO.MailRequestDTOBuilder recipients(List<String> recipients) {
      if (!recipients.isEmpty()) {
        this.recipients.addAll(recipients);
      }
      return this;
    }

    public MailRequestDTO.MailRequestDTOBuilder recipient(String recipient) {
      if (recipient != null) {
        this.recipients.add(recipient);
      }
      return this;
    }

    public MailRequestDTO.MailRequestDTOBuilder config(ConfigDTO config) {
      this.config = config;
      return this;
    }

    public MailRequestDTO build() {
      MailRequestDTO mailRequestDTO = new MailRequestDTO(this);
      return mailRequestDTO;
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
    MailRequestDTO mailRequestDTO = (MailRequestDTO) o;
    return Objects.equals(this.sender, mailRequestDTO.sender) && Objects.equals(this.recipients, mailRequestDTO.recipients) && Objects.equals(this.config, mailRequestDTO.config);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, recipients, config);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MailRequestDTO{");
    sb.append(" sender:").append(sender).append(",");
    sb.append(" recipients:").append(recipients).append(",");
    sb.append(" config:").append(config);
    sb.append("}");
    return sb.toString();
  }


}
