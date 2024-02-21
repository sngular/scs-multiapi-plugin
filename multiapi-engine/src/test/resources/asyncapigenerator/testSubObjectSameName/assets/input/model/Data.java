package input.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Data.DataBuilder.class)
public class Data {

  @JsonProperty(value ="commitId")
  private String commitId;

  private Data(DataBuilder builder) {
    this.commitId = builder.commitId;

  }

  public static Data.DataBuilder builder() {
    return new Data.DataBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class DataBuilder {

    private String commitId;

    public Data.DataBuilder commitId(String commitId) {
      this.commitId = commitId;
      return this;
    }

    public Data build() {
      Data data = new Data(this);
      return data;
    }
  }

  /**
  * Get commitId
  * @return commitId
  */
  @Schema(name = "commitId", required = false)
  public String getCommitId() {
    return commitId;
  }
  public void setCommitId(String commitId) {
    this.commitId = commitId;
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
    return Objects.equals(this.commitId, data.commitId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(commitId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Data{");
    sb.append(" commitId:").append(commitId);
    sb.append("}");
    return sb.toString();
  }


}
