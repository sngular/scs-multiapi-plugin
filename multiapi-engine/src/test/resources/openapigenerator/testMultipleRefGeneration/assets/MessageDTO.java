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
  private  message;

  private MessageDTO(String description, message) {
    this.description = description;
    this.message = message;

  }

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
    private  message;

    public MessageDTO.MessageDTOBuilder description(String description) {
      this.description = description;
      return this;
    }
    public MessageDTO.MessageDTOBuilder message( message) {
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
  public  getMessage() {
    return message;
  }
  public void setMessage( message) {
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
    sb.append("class MessageDTO {\n");
    sb.append(" description: ").append(toIndentedString(description)).append("\n");
    sb.append(" message: ").append(toIndentedString(message)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }


}
