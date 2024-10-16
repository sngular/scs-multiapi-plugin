package output.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Data.DataBuilder.class)
public class Data {

  @JsonProperty(value ="name")
  private String name;
  @JsonProperty(value ="tenantId")
  private String tenantId;

  private Data(DataBuilder builder) {
    this.name = builder.name;
    this.tenantId = builder.tenantId;

  }

  public static Data.DataBuilder builder() {
    return new Data.DataBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataBuilder {

    private String name;
    private String tenantId;

    public Data.DataBuilder name(String name) {
      this.name = name;
      return this;
    }

    public Data.DataBuilder tenantId(String tenantId) {
      this.tenantId = tenantId;
      return this;
    }

    public Data build() {
      Data data = new Data(this);
      return data;
    }
  }

  @Schema(name = "name", required = false)
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  @Schema(name = "tenantId", required = false)
  public String getTenantId() {
    return tenantId;
  }
  public void setTenantId(String tenantId) {
    this.tenantId = tenantId;
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
    return Objects.equals(this.name, data.name) && Objects.equals(this.tenantId, data.tenantId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, tenantId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Data{");
    sb.append(" name:").append(name).append(",");
    sb.append(" tenantId:").append(tenantId);
    sb.append("}");
    return sb.toString();
  }


}
