package com.sngular.multifileplugin.multipleref.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder.class)
public class InlineResponse200CreateGameDTO {

  @JsonProperty(value ="description")
  private String description;
  @JsonProperty(value ="code")
  private Integer code;
  @JsonProperty(value ="message")
  private MessageDTO message;

  private InlineResponse200CreateGameDTO(String description, Integer code, MessageDTO message) {
    this.description = description;
    this.code = code;
    this.message = message;

  }

  private InlineResponse200CreateGameDTO(InlineResponse200CreateGameDTOBuilder builder) {
    this.description = builder.description;
    this.code = builder.code;
    this.message = builder.message;

  }

  public static InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder builder() {
    return new InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class InlineResponse200CreateGameDTOBuilder {

    private String description;
    private Integer code;
    private MessageDTO message;

    public InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder description(String description) {
      this.description = description;
      return this;
    }

    public InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }
    public InlineResponse200CreateGameDTO.InlineResponse200CreateGameDTOBuilder message(MessageDTO message) {
      this.message = message;
      return this;
    }

    public InlineResponse200CreateGameDTO build() {
      InlineResponse200CreateGameDTO inlineResponse200CreateGameDTO = new InlineResponse200CreateGameDTO(this);
      return inlineResponse200CreateGameDTO;
    }
  }

  @Schema(name = "description", required = false)
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }

  @Schema(name = "code", required = false)
  public Integer getCode() {
    return code;
  }
  public void setCode(Integer code) {
    this.code = code;
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
    InlineResponse200CreateGameDTO inlineResponse200CreateGameDTO = (InlineResponse200CreateGameDTO) o;
    return Objects.equals(this.description, inlineResponse200CreateGameDTO.description) && Objects.equals(this.code, inlineResponse200CreateGameDTO.code) && Objects.equals(this.message, inlineResponse200CreateGameDTO.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(description, code, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("InlineResponse200CreateGameDTO{");
    sb.append(" description:").append(description).append(",");
    sb.append(" code:").append(code).append(",");
    sb.append(" message:").append(message).append(",");
    sb.append("}");
    return sb.toString();
  }


}
