package smartylighting.streetlights.messaging.producer.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import smartylighting.streetlights.messaging.producer.model.customvalidator.Min;

@JsonDeserialize(builder = LightMeasuredPayloadDTO.LightMeasuredPayloadDTOBuilder.class)
public class LightMeasuredPayloadDTO {

  @JsonProperty(value ="lumens")
  @Min(minimum = "0", exclusive = false)
  private Integer lumens;
  @JsonProperty(value ="sentAt")
  private SentAtDTO sentAt;

  private LightMeasuredPayloadDTO(Integer lumens, SentAtDTO sentAt) {
    this.lumens = lumens;
    this.sentAt = sentAt;

  }

  private LightMeasuredPayloadDTO(LightMeasuredPayloadDTOBuilder builder) {
    this.lumens = builder.lumens;
    this.sentAt = builder.sentAt;

  }

  public static LightMeasuredPayloadDTO.LightMeasuredPayloadDTOBuilder builder() {
    return new LightMeasuredPayloadDTO.LightMeasuredPayloadDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class LightMeasuredPayloadDTOBuilder {

    private Integer lumens;
    private SentAtDTO sentAt;

    public LightMeasuredPayloadDTO.LightMeasuredPayloadDTOBuilder lumens(Integer lumens) {
      this.lumens = lumens;
      return this;
    }

    public LightMeasuredPayloadDTO.LightMeasuredPayloadDTOBuilder sentAt(SentAtDTO sentAt) {
      this.sentAt = sentAt;
      return this;
    }

    public LightMeasuredPayloadDTO build() {
      LightMeasuredPayloadDTO lightMeasuredPayloadDTO = new LightMeasuredPayloadDTO(this);
      return lightMeasuredPayloadDTO;
    }
  }

  /**
  * Get lumens
  * @return lumens
  */
  @Schema(name = "lumens", required = false)
  public Integer getLumens() {
    return lumens;
  }
  public void setLumens(Integer lumens) {
    this.lumens = lumens;
  }

  /**
  * Get sentAt
  * @return sentAt
  */
  @Schema(name = "sentAt", required = false)
  public SentAtDTO getSentAt() {
    return sentAt;
  }
  public void setSentAt(SentAtDTO sentAt) {
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
    LightMeasuredPayloadDTO lightMeasuredPayloadDTO = (LightMeasuredPayloadDTO) o;
    return Objects.equals(this.lumens, lightMeasuredPayloadDTO.lumens) && Objects.equals(this.sentAt, lightMeasuredPayloadDTO.sentAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(lumens, sentAt);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("LightMeasuredPayloadDTO{");
    sb.append(" lumens:").append(lumens).append(",");
    sb.append(" sentAt:").append(sentAt);
    sb.append("}");
    return sb.toString();
  }




}
