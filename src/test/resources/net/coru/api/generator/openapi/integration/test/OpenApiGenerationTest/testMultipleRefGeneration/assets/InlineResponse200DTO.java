package net.coru.multifileplugin.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;

public class InlineResponse200DTO {

  @JsonProperty(value ="message")
  private MessageDTO message;
  @JsonProperty(value ="code")
  private Integer code;

  private InlineResponse200DTO(InlineResponse200DTOBuilder builder) {
    this.message = builder.message;
    this.code = builder.code;
  }

  public static class InlineResponse200DTOBuilder {

    private MessageDTO message;
    private Integer code;

    public InlineResponse200DTO.InlineResponse200DTOBuilder message(MessageDTO message) {
      this.message = message;
      return this;
    }

    public InlineResponse200DTO.InlineResponse200DTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public InlineResponse200DTO build() {
      InlineResponse200DTO inlineResponse200DTO =  new InlineResponse200DTO(this);
      return inlineResponse200DTO;
    }
  }

  /**
  * Get message
  * @return message
  */
  @ApiModelProperty( value = "description")
  public MessageDTO getMessage() {
    return message;
  }
  public void setMessage(MessageDTO message) {
    this.message = message;
  }

  /**
  * Get code
  * @return code
  */
  @ApiModelProperty( value = "description")
  public Integer getCode() {
    return code;
  }
  public void setCode(Integer code) {
    this.code = code;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    InlineResponse200DTO inlineResponse200DTO = (InlineResponse200DTO) o;
    return Objects.equals(this.message,inlineResponse200DTO.message) && Objects.equals(this.code,inlineResponse200DTO.code) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(message,code);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineResponse200DTO {\n");
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

}
