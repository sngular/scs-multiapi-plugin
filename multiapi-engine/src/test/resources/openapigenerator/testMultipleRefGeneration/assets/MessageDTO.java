package com.sngular.multifileplugin.multipleref.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = MessageDTO.MessageDTOBuilder.class)
public class MessageDTO {

  @JsonProperty(value ="description")
  private String description;
  @JsonProperty(value ="message")
  private MessageDTO message;

  private MessageDTO(MessageDTOBuilder builder) {
    this.description = builder.description;
    this.message = builder.message;

  }

  public static MessageDTO.MessageDTOBuilder builder() {
    return new MessageDTO.MessageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class MessageDTOBuilder {

    private String description;
    private MessageDTO message;

    public MessageDTO.MessageDTOBuilder description(String description) {
      this.description = description;
      return this;
    }

    public MessageDTO.MessageDTOBuilder message(MessageDTO message) {
      this.message = message;
      return this;
    }

    public MessageDTO build() {
      MessageDTO messageDTO = new MessageDTO(this);
      return messageDTO;
    }
  }

  @Schema(name = "description", required = false)
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  @Schema(name = "message", required = false)
  public MessageDTO getMessage() {
    return message;
  }
  public void setMessage(MessageDTO message) {
    this.message = message;
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
    return Objects.equals(this.description, messageDTO.description) && Objects.equals(this.message, messageDTO.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(description, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("MessageDTO{");
    sb.append(" description:").append(description).append(",");
    sb.append(" message:").append(message);
    sb.append("}");
    return sb.toString();
  }


}
