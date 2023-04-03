package company.mail.model.schemas;

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
  @JsonProperty(value ="configurationDTO")
  private company.mail.model.schemas.ConfigurationDTO configurationDTO;

  private MailRequestDTO(String sender, List<String> recipients, company.mail.model.schemas.ConfigurationDTO configurationDTO) {
    this.sender = sender;
    this.recipients = recipients;
    this.configurationDTO = configurationDTO;

  }

  private MailRequestDTO(MailRequestDTOBuilder builder) {
    this.sender = builder.sender;
    this.recipients = builder.recipients;
    this.configurationDTO = builder.configurationDTO;

  }

  public static MailRequestDTO.MailRequestDTOBuilder builder() {
    return new MailRequestDTO.MailRequestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestDTOBuilder {

    private String sender;
    private List<String> recipients = new ArrayList<String>();
    private company.mail.model.schemas.ConfigurationDTO configurationDTO;

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

    public MailRequestDTO.MailRequestDTOBuilder configurationDTO(company.mail.model.schemas.ConfigurationDTO configurationDTO) {
      this.configurationDTO = configurationDTO;
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
  * Get configurationDTO
  * @return configurationDTO
  */
  @Schema(name = "configurationDTO", required = false)
  public company.mail.model.schemas.ConfigurationDTO getConfigurationDTO() {
    return configurationDTO;
  }
  public void setConfigurationDTO(company.mail.model.schemas.ConfigurationDTO configurationDTO) {
    this.configurationDTO = configurationDTO;
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
    return Objects.equals(this.sender, mailRequestDTO.sender) && Objects.equals(this.recipients, mailRequestDTO.recipients) && Objects.equals(this.configurationDTO, mailRequestDTO.configurationDTO);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sender, recipients, configurationDTO);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MailRequestDTO {\n");
    sb.append(" sender: ").append(toIndentedString(sender)).append("\n");
    sb.append(" recipients: ").append(toIndentedString(recipients)).append("\n");
    sb.append(" configurationDTO: ").append(toIndentedString(configurationDTO)).append("\n");
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
