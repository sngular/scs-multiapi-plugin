package smartylighting.streetlights.messaging.consumer.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = TurnOnOffPayload.TurnOnOffPayloadBuilder.class)
public class TurnOnOffPayload {

  @JsonProperty(value ="command")
  private Command command;
  public enum Command {
    TRUE("true"),
    FALSE("false");

    private String value;

    Command(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="sentAt")
  private SentAt sentAt;

  private TurnOnOffPayload(Command command, SentAt sentAt) {
    this.command = command;
    this.sentAt = sentAt;

  }

  private TurnOnOffPayload(TurnOnOffPayloadBuilder builder) {
    this.command = builder.command;
    this.sentAt = builder.sentAt;

  }

  public static TurnOnOffPayload.TurnOnOffPayloadBuilder builder() {
    return new TurnOnOffPayload.TurnOnOffPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TurnOnOffPayloadBuilder {

    private Command command;
    private SentAt sentAt;
    public TurnOnOffPayload.TurnOnOffPayloadBuilder command(Command command) {
      this.command = command;
      return this;
    }

    public TurnOnOffPayload.TurnOnOffPayloadBuilder sentAt(SentAt sentAt) {
      this.sentAt = sentAt;
      return this;
    }

    public TurnOnOffPayload build() {
      TurnOnOffPayload turnOnOffPayload = new TurnOnOffPayload(this);
      return turnOnOffPayload;
    }
  }

  /**
  * Get command
  * @return command
  */
  @Schema(name = "command", required = false)
  public Command getCommand() {
    return command;
  }
  public void setCommand(Command command) {
    this.command = command;
  }

  /**
  * Get sentAt
  * @return sentAt
  */
  @Schema(name = "sentAt", required = false)
  public SentAt getSentAt() {
    return sentAt;
  }
  public void setSentAt(SentAt sentAt) {
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
    TurnOnOffPayload turnOnOffPayload = (TurnOnOffPayload) o;
    return Objects.equals(this.command, turnOnOffPayload.command) && Objects.equals(this.sentAt, turnOnOffPayload.sentAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, sentAt);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("TurnOnOffPayload{");
    sb.append(" command:").append(command).append(",");
    sb.append(" sentAt:").append(sentAt);
    sb.append("}");
    return sb.toString();
  }




}
