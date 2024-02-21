package output.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Output.OutputBuilder.class)
public class Output {

  @JsonProperty(value ="source")
  private Source source;
  public enum Source {
    TENANT("tenant");

    private String value;

    Source(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="type")
  private Type type;
  public enum Type {
    UPDATE("update"),
    DELETE("delete");

    private String value;

    Type(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="data")
  private Data data;

  private Output(OutputBuilder builder) {
    this.source = builder.source;
    this.type = builder.type;
    this.data = builder.data;

  }

  public static Output.OutputBuilder builder() {
    return new Output.OutputBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class OutputBuilder {

    private Source source;

    private Type type;

    private Data data;

    public Output.OutputBuilder source(Source source) {
      this.source = source;
      return this;
    }

    public Output.OutputBuilder type(Type type) {
      this.type = type;
      return this;
    }

    public Output.OutputBuilder data(Data data) {
      this.data = data;
      return this;
    }

    public Output build() {
      Output output = new Output(this);
      return output;
    }
  }

  /**
  * Get source
  * @return source
  */
  @Schema(name = "source", required = false)
  public Source getSource() {
    return source;
  }
  public void setSource(Source source) {
    this.source = source;
  }

  /**
  * Get type
  * @return type
  */
  @Schema(name = "type", required = false)
  public Type getType() {
    return type;
  }
  public void setType(Type type) {
    this.type = type;
  }

  /**
  * Get data
  * @return data
  */
  @Schema(name = "data", required = false)
  public Data getData() {
    return data;
  }
  public void setData(Data data) {
    this.data = data;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Output output = (Output) o;
    return Objects.equals(this.source, output.source) && Objects.equals(this.type, output.type) && Objects.equals(this.data, output.data);
  }

  @Override
  public int hashCode() {
    return Objects.hash(source, type, data);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Output{");
    sb.append(" source:").append(source).append(",");
    sb.append(" type:").append(type).append(",");
    sb.append(" data:").append(data);
    sb.append("}");
    return sb.toString();
  }


}
