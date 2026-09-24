package com.sngular.scsplugin.streetlights.model.event;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = TurnOnOffPayloadDTO.TurnOnOffPayloadDTOBuilder.class)
public class TurnOnOffPayloadDTO {

  @JsonProperty(value ="command")
  private Command command;
  public enum Command {
    TRUE("true"),
    FALSE("false"),
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}. */
    UNKNOWN("UNKNOWN");

    private String value;

    Command(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static Command fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (Command constant : values()) {
        if (value.equals(constant.value)) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="sentAt")
  private Object sentAt;

  private TurnOnOffPayloadDTO(TurnOnOffPayloadDTOBuilder builder) {
    this.command = builder.command;
    this.sentAt = builder.sentAt;

  }

  public static TurnOnOffPayloadDTO.TurnOnOffPayloadDTOBuilder builder() {
    return new TurnOnOffPayloadDTO.TurnOnOffPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TurnOnOffPayloadDTOBuilder {

    private Command command;
    private Object sentAt;

    public TurnOnOffPayloadDTO.TurnOnOffPayloadDTOBuilder command(Command command) {
      this.command = command;
      return this;
    }

    public TurnOnOffPayloadDTO.TurnOnOffPayloadDTOBuilder sentAt(Object sentAt) {
      this.sentAt = sentAt;
      return this;
    }

    public TurnOnOffPayloadDTO build() {
      TurnOnOffPayloadDTO turnOnOffPayloadDTO = new TurnOnOffPayloadDTO(this);
      return turnOnOffPayloadDTO;
    }
  }

  @Schema(name = "command", required = false, description = "Whether to turn on or off the light.")
  public Command getCommand() {
    return command;
  }
  public void setCommand(Command command) {
    this.command = command;
  }

  @Schema(name = "sentAt", required = false)
  public Object getSentAt() {
    return sentAt;
  }
  public void setSentAt(Object sentAt) {
    this.sentAt = sentAt;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TurnOnOffPayloadDTO turnOnOffPayloadDTO = (TurnOnOffPayloadDTO) o;
    return Objects.equals(this.command, turnOnOffPayloadDTO.command) && Objects.equals(this.sentAt, turnOnOffPayloadDTO.sentAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, sentAt);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("TurnOnOffPayloadDTO{");
    sb.append(" command:").append(command).append(",");
    sb.append(" sentAt:").append(sentAt);
    sb.append("}");
    return sb.toString();
  }


}
