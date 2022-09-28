package net.coru.multifileplugin.pathparameter.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import net.coru.multifileplugin.pathparameter.model.exception.ModelClassException;


public class ErrorDTO {

  @JsonProperty(value ="message")
  private final String message;
  @JsonProperty(value ="code")
  private final Integer code;

  private ErrorDTO(String message, Integer code) {
    this.message = message;
    this.code = code;

    validateRequiredAttributes();
  }

  private ErrorDTO(ErrorDTOBuilder builder) {
    this.message = builder.message;
    this.code = builder.code;

    validateRequiredAttributes();
  }

  public static class ErrorDTOBuilder {

    private String message;
    private Integer code;

    public ErrorDTO.ErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ErrorDTO.ErrorDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public ErrorDTO build() {
      ErrorDTO errorDTO = new ErrorDTO(this);
      return errorDTO;
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
    ErrorDTO errorDTO = (ErrorDTO) o;
    return Objects.equals(this.message, errorDTO.message) && Objects.equals(this.code, errorDTO.code);
  }

  @Override
  public int hashCode() {
    return Objects.hash(message, code);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ErrorDTO {\n");
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
      throw new ModelClassException("ErrorDTO");
    }
  }

}
