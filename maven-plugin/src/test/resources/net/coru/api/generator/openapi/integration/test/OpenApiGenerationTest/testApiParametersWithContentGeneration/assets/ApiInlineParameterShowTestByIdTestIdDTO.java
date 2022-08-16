package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiParametersWithContentGeneration.assets;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;


public class ApiInlineParameterShowTestByIdTestIdDTO {

  @JsonProperty(value ="email")
  private String email;

  private ApiInlineParameterShowTestByIdTestIdDTO(String email){
    this.email = email;

  }

  private ApiInlineParameterShowTestByIdTestIdDTO(ApiInlineParameterShowTestByIdTestIdDTOBuilder builder) {
    this.email = builder.email;

  }

  public static class ApiInlineParameterShowTestByIdTestIdDTOBuilder {

    private String email;

    public ApiInlineParameterShowTestByIdTestIdDTO.ApiInlineParameterShowTestByIdTestIdDTOBuilder email(String email) {
      this.email = email;
      return this;
    }

    public ApiInlineParameterShowTestByIdTestIdDTO build() {
      ApiInlineParameterShowTestByIdTestIdDTO apiInlineParameterShowTestByIdTestIdDTO =  new ApiInlineParameterShowTestByIdTestIdDTO(this);
      return apiInlineParameterShowTestByIdTestIdDTO;
    }
  }

  /**
  * Get email
  * @return email
  */
  @Schema(name = "email", required = false)
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiInlineParameterShowTestByIdTestIdDTO apiInlineParameterShowTestByIdTestIdDTO = (ApiInlineParameterShowTestByIdTestIdDTO) o;
    return Objects.equals(this.email,apiInlineParameterShowTestByIdTestIdDTO.email) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(email);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiInlineParameterShowTestByIdTestIdDTO {\n");
    sb.append(" email: ").append(toIndentedString(email)).append("\n");
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
