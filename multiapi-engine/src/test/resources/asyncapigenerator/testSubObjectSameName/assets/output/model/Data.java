package output.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Data.DataBuilder.class)
public class Data {

  @JsonProperty(value ="tenantId")
  private String tenantId;
  @JsonProperty(value ="name")
  private String name;

  private Data(DataBuilder builder) {
    this.tenantId = builder.tenantId;
    this.name = builder.name;

  }

  public static Data.DataBuilder builder() {
    return new Data.DataBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataBuilder {

    private String tenantId;

    private String name;

    public Data.DataBuilder tenantId(String tenantId) {
      this.tenantId = tenantId;
      return this;
    }

    public Data.DataBuilder name(String name) {
      this.name = name;
      return this;
    }

    public Data build() {
      Data data = new Data(this);
      return data;
    }
  }

  /**
  * Get tenantId
  * @return tenantId
  */
  @Schema(name = "tenantId", required = false)
  public String getTenantId() {
    return tenantId;
  }
  public void setTenantId(String tenantId) {
    this.tenantId = tenantId;
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
    Data data = (Data) o;
    return Objects.equals(this.tenantId, data.tenantId) && Objects.equals(this.name, data.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(tenantId, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Data{");
    sb.append(" tenantId:").append(tenantId).append(",");
    sb.append(" name:").append(name);
    sb.append("}");
    return sb.toString();
  }


}
