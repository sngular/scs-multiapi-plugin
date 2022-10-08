package net.coru.scsplugin.business_model.model.event;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.scsplugin.business_model.model.event.CreateOrderPayloadMapper;
import net.coru.scsplugin.business_model.model.event.exception.ModelClassException;

public class CreateOrderMapper {

  @JsonProperty(value ="payload")
  private final CreateOrderPayloadMapper payload;

  private CreateOrderMapper(CreateOrderPayloadMapper payload) {
    this.payload = payload;

    validateRequiredAttributes();
  }

  private CreateOrderMapper(CreateOrderMapperBuilder builder) {
    this.payload = builder.payload;

    validateRequiredAttributes();
  }

  public static CreateOrderMapper.CreateOrderMapperBuilder builder() {
    return new CreateOrderMapper.CreateOrderMapperBuilder();
  }

  public static class CreateOrderMapperBuilder {

    private CreateOrderPayloadMapper payload;

    public CreateOrderMapper.CreateOrderMapperBuilder payload(CreateOrderPayloadMapper payload) {
      this.payload = payload;
      return this;
    }

    public CreateOrderMapper build() {
      CreateOrderMapper createOrderMapper = new CreateOrderMapper(this);
      return createOrderMapper;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = true)
  public CreateOrderPayloadMapper getPayload() {
    return payload;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CreateOrderMapper createOrderMapper = (CreateOrderMapper) o;
    return Objects.equals(this.payload, createOrderMapper.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CreateOrderMapper {\n");
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


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.payload)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("CreateOrderMapper");
    }
  }

}
