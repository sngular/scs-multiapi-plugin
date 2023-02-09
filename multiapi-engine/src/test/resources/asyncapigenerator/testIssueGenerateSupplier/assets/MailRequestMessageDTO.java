package company.mail.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import company.mail.model.MailRequestDTO;

@JsonDeserialize(builder = MailRequestMessageDTO.MailRequestMessageDTOBuilder.class)
public class MailRequestMessageDTO {

  @JsonProperty(value ="payload")
  private MailRequestDTO payload;

  private MailRequestMessageDTO(MailRequestDTO payload) {
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

    private MailRequestDTO payload;

    public MailRequestMessageDTO.MailRequestMessageDTOBuilder payload(MailRequestDTO payload) {
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
  public MailRequestDTO getPayload() {
    return payload;
  }
  public void setPayload(MailRequestDTO payload) {
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
    sb.append("class MailRequestMessageDTO {\n");
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
