package company.mail.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = MailRequestMessageDTO.MailRequestMessageDTOBuilder.class)
public class MailRequestMessageDTO {

  @JsonProperty(value ="payload")
  private company.mail.model.schemas.MailRequestDTO payload;

  private MailRequestMessageDTO(company.mail.model.schemas.MailRequestDTO payload) {
    this.payload = payload;

  }

  private MailRequestMessageDTO(MailRequestMessageDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static MailRequestMessageDTO.MailRequestMessageDTOBuilder builder() {
    return new MailRequestMessageDTO.MailRequestMessageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MailRequestMessageDTOBuilder {

    private company.mail.model.schemas.MailRequestDTO payload;

    public MailRequestMessageDTO.MailRequestMessageDTOBuilder payload(company.mail.model.schemas.MailRequestDTO payload) {
      this.payload = payload;
      return this;
    }

    public MailRequestMessageDTO build() {
      MailRequestMessageDTO mailRequestMessageDTO = new MailRequestMessageDTO(this);
      return mailRequestMessageDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public company.mail.model.schemas.MailRequestDTO getPayload() {
    return payload;
  }
  public void setPayload(company.mail.model.schemas.MailRequestDTO payload) {
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
    MailRequestMessageDTO mailRequestMessageDTO = (MailRequestMessageDTO) o;
    return Objects.equals(this.payload, mailRequestMessageDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MailRequestMessageDTO{");
    sb.append(" payload:").append(toIndentedString(payload)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
