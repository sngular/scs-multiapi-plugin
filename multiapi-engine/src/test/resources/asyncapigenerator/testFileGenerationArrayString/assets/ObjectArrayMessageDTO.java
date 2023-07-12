package com.sngular.scsplugin.arraywithstring.model.event.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder.class)
public class ObjectArrayMessageDTO {

  @JsonProperty(value ="payload")
  private com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO payload;

  private ObjectArrayMessageDTO(com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO payload) {
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

    private com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO payload;

    public ObjectArrayMessageDTO.ObjectArrayMessageDTOBuilder payload(com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO payload) {
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
  public com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.scsplugin.arraywithstring.model.event.schemas.ObjectArrayDTO payload) {
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
    sb.append("ObjectArrayMessageDTO{");
    sb.append(" payload:").append(toIndentedString(payload)).append(",");
    sb.append("}");
    return sb.toString();
  }




}
