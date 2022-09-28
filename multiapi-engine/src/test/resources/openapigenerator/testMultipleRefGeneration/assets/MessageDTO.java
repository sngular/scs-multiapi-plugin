package net.coru.multifileplugin.multipleref.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

public class MessageDTO {

  @JsonProperty(value ="description")
  private String description;

  private MessageDTO(String description) {
    this.description = description;

  }

  private MessageDTO(MessageDTOBuilder builder) {
    this.description = builder.description;

  }

  public static class MessageDTOBuilder {

    private String description;

    public MessageDTO.MessageDTOBuilder description(String description) {
      this.description = description;
      return this;
    }

    public MessageDTO build() {
      MessageDTO messageDTO = new MessageDTO(this);
      return messageDTO;
    }
  }

  /**
  * Get description
  * @return description
  */
  @Schema(name = "description", required = false)
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MessageDTO messageDTO = (MessageDTO) o;
    return Objects.equals(this.description, messageDTO.description);
  }

  @Override
  public int hashCode() {
    return Objects.hash(description);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MessageDTO {\n");
    sb.append(" description: ").append(toIndentedString(description)).append("\n");
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
