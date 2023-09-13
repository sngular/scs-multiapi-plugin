package smartylighting.streetlights.messaging.consumer.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import smartylighting.streetlights.messaging.consumer.model.customvalidator.Max;
import smartylighting.streetlights.messaging.consumer.model.customvalidator.Min;

@JsonDeserialize(builder = DimLightPayload.DimLightPayloadBuilder.class)
public class DimLightPayload {

  @JsonProperty(value ="percentage")
  @Min(minimum = "0", exclusive = false)
  @Max(maximum = "100", exclusive = false)
  private Integer percentage;
  @JsonProperty(value ="sentAt")
  private SentAt sentAt;

  private DimLightPayload(Integer percentage, SentAt sentAt) {
    this.percentage = percentage;
    this.sentAt = sentAt;

  }

  private DimLightPayload(DimLightPayloadBuilder builder) {
    this.percentage = builder.percentage;
    this.sentAt = builder.sentAt;

  }

  public static DimLightPayload.DimLightPayloadBuilder builder() {
    return new DimLightPayload.DimLightPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DimLightPayloadBuilder {

    private Integer percentage;
    private SentAt sentAt;

    public DimLightPayload.DimLightPayloadBuilder percentage(Integer percentage) {
      this.percentage = percentage;
      return this;
    }

    public DimLightPayload.DimLightPayloadBuilder sentAt(SentAt sentAt) {
      this.sentAt = sentAt;
      return this;
    }

    public DimLightPayload build() {
      DimLightPayload dimLightPayload = new DimLightPayload(this);
      return dimLightPayload;
    }
  }

  /**
  * Get percentage
  * @return percentage
  */
  @Schema(name = "percentage", required = false)
  public Integer getPercentage() {
    return percentage;
  }
  public void setPercentage(Integer percentage) {
    this.percentage = percentage;
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
    DimLightPayload dimLightPayload = (DimLightPayload) o;
    return Objects.equals(this.percentage, dimLightPayload.percentage) && Objects.equals(this.sentAt, dimLightPayload.sentAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(percentage, sentAt);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("DimLightPayload{");
    sb.append(" percentage:").append(percentage).append(",");
    sb.append(" sentAt:").append(sentAt);
    sb.append("}");
    return sb.toString();
  }




}
