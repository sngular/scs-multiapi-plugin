package net.coru.scsplugin.issuegeneration.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.issuegeneration.model.event.StatusMsgDTO;

public class StatusDTO {

  @JsonProperty(value ="payload")
  private StatusMsgDTO payload;

  private StatusDTO(StatusMsgDTO payload) {
    this.payload = payload;

  }

  private StatusDTO(StatusDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static StatusDTO.StatusDTOBuilder builder() {
    return new StatusDTO.StatusDTOBuilder();
  }

  public static class StatusDTOBuilder {

    private StatusMsgDTO payload;

    public StatusDTO.StatusDTOBuilder payload(StatusMsgDTO payload) {
      this.payload = payload;
      return this;
    }

    public StatusDTO build() {
      StatusDTO statusDTO = new StatusDTO(this);
      return statusDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public StatusMsgDTO getPayload() {
    return payload;
  }
  public void setPayload(StatusMsgDTO payload) {
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
    StatusDTO statusDTO = (StatusDTO) o;
    return Objects.equals(this.payload, statusDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class StatusDTO {\n");
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
