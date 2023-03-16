package com.sngular.scsplugin.arraywithstring.model.event.message;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.scsplugin.arraywithstring.model.event.schema.ObjectArrayDTO;

@JsonDeserialize(builder = ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder.class)
public class ObjectArrayMessageDTO {

  @JsonProperty(value ="payload")
  private ObjectArrayDTO payload;

  private ObjectArrayMessageDTO(ObjectArrayDTO payload) {
    this.payload = payload;

  }

  private ObjectArrayMessageDTO(ObjectArrayMessageDTOBuilder builder) {
    this.payload = builder.payload;

  }

  public static ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder builder() {
    return new ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ObjectArrayMessageDTOBuilder {

    private ObjectArrayDTO payload;

    public ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder payload(ObjectArrayDTO payload) {
      this.payload = payload;
      return this;
    }

    public ObjectArrayMessageDTO build() {
      ObjectArrayMessageDTO objectArrayMessageDTO = new ObjectArrayMessageDTO(this);
      return objectArrayMessageDTO;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public ObjectArrayDTO getPayload() {
    return payload;
  }
  public void setPayload(ObjectArrayDTO payload) {
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
    ObjectArrayMessageDTO objectArrayMessageDTO = (ObjectArrayMessageDTO) o;
    return Objects.equals(this.payload, objectArrayMessageDTO.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ObjectArrayMessageDTO {\n");
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
