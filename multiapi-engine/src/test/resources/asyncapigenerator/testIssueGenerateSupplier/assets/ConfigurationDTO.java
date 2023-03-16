package company.mail.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = ConfigurationDTO.ConfigurationDTOBuilder.class)
public class ConfigurationDTO {

  @JsonProperty(value ="name")
  private String name;

  private ConfigurationDTO(String name) {
    this.name = name;

  }

  private ConfigurationDTO(ConfigurationDTOBuilder builder) {
    this.name = builder.name;

  }

  public static ConfigurationDTO.ConfigurationDTOBuilder builder() {
    return new ConfigurationDTO.ConfigurationDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ConfigurationDTOBuilder {

    private String name;

    public ConfigurationDTO.ConfigurationDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ConfigurationDTO build() {
      ConfigurationDTO configurationDTO = new ConfigurationDTO(this);
      return configurationDTO;
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ConfigurationDTO configurationDTO = (ConfigurationDTO) o;
    return Objects.equals(this.name, configurationDTO.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ConfigurationDTO {\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
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
