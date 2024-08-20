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
  @JsonProperty(value ="configuration")
  private ConfigurationDTO configuration;
  @JsonProperty(value ="recipients")
  private List<String> recipients;

  private MailRequestDTO(MailRequestDTOBuilder builder) {
    this.sender = builder.sender;
    this.configuration = builder.configuration;
    this.recipients = builder.recipients;

  }

  public static MailRequestDTO.MailRequestDTOBuilder builder() {
    return new MailRequestDTO.MailRequestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestDTOBuilder {

    private String sender;
    private ConfigurationDTO configuration;
    private List<String> recipients = new ArrayList<String>();

    public MailRequestDTO.MailRequestDTOBuilder sender(String sender) {
      this.sender = sender;
      return this;
    }

    public MailRequestDTO.MailRequestDTOBuilder configuration(ConfigurationDTO configuration) {
      this.configuration = configuration;
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

    public MailRequestDTO build() {
      MailRequestDTO mailRequestDTO = new MailRequestDTO(this);
      return mailRequestDTO;
    }
  }

  @Schema(name = "sender", required = false)
  public String getSender() {
    return sender;
  }
  public void setSender(String sender) {
    this.sender = sender;
  }

  @Schema(name = "configuration", required = false)
  public ConfigurationDTO getConfiguration() {
    return configuration;
  }
  public void setConfiguration(ConfigurationDTO configuration) {
    this.configuration = configuration;
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
    MailRequestDTO mailRequestDTO = (MailRequestDTO) o;
    return Objects.equals(this.sender, mailRequestDTO.sender) && Objects.equals(this.configuration, mailRequestDTO.configuration) && Objects.equals(this.recipients, mailRequestDTO.recipients);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, configuration, recipients);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MailRequestDTO{");
    sb.append(" sender:").append(sender).append(",");
    sb.append(" configuration:").append(configuration).append(",");
    sb.append(" recipients:").append(recipients);
    sb.append("}");
    return sb.toString();
  }


}
