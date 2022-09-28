package net.coru.multifileplugin.parameterwithcontent.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.multifileplugin.parameterwithcontent.model.exception.ModelClassException;

public class ApiErrorDTO {

  @JsonProperty(value ="message")
  private final String message;
  @JsonProperty(value ="code")
  private final Integer code;

  private ApiErrorDTO(String message, Integer code) {
    this.message = message;
    this.code = code;

    validateRequiredAttributes();
  }

  private ApiErrorDTO(ApiErrorDTOBuilder builder) {
    this.message = builder.message;
    this.code = builder.code;

    validateRequiredAttributes();
  }

  public static class ApiErrorDTOBuilder {

    private String message;
    private Integer code;

    public ApiErrorDTO.ApiErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public ApiErrorDTO build() {
      ApiErrorDTO apiErrorDTO = new ApiErrorDTO(this);
      return apiErrorDTO;
    }
  }

  /**
  * Get message
  * @return message
  */
  @Schema(name = "message", required = true)
  public String getMessage() {
    return message;
  }

  /**
  * Get code
  * @return code
  */
  @Schema(name = "code", required = true)
  public Integer getCode() {
    return code;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiErrorDTO apiErrorDTO = (ApiErrorDTO) o;
    return Objects.equals(this.message, apiErrorDTO.message) && Objects.equals(this.code, apiErrorDTO.code);
  }

  @Override
  public int hashCode() {
    return Objects.hash(message, code);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiErrorDTO {\n");
    sb.append(" message: ").append(toIndentedString(message)).append("\n");
    sb.append(" code: ").append(toIndentedString(code)).append("\n");
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

    if (!Objects.nonNull(this.message)) {
      satisfiedCondition = false;
    }
    else if (!Objects.nonNull(this.code)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiErrorDTO");
    }
  }

}
