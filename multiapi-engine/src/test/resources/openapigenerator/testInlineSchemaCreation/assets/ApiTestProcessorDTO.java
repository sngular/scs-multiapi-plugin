package net.coru.multifileplugin.inlineschemacreation.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;


public class ApiTestProcessorDTO {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="id")
  private String id;

  private ApiTestProcessorDTO(String name, String id) {
    this.name = name;
    this.id = id;

  }

  private ApiTestProcessorDTO(ApiTestProcessorDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

  }

  public static class ApiTestProcessorDTOBuilder {

    private String name;
    private String id;

    public ApiTestProcessorDTO.ApiTestProcessorDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestProcessorDTO.ApiTestProcessorDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public ApiTestProcessorDTO build() {
      ApiTestProcessorDTO apiTestProcessorDTO = new ApiTestProcessorDTO(this);
      return apiTestProcessorDTO;
    }
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestProcessorDTO apiTestProcessorDTO = (ApiTestProcessorDTO) o;
    return Objects.equals(this.name, apiTestProcessorDTO.name) && Objects.equals(this.id, apiTestProcessorDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestProcessorDTO {\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
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
