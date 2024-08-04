package input.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = Input.InputBuilder.class)
public class Input {

  @JsonProperty(value ="source")
  private Source source;
  public enum Source {
    INPUT("input");

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
  @JsonProperty(value ="data")
  private Data data;

  private Input(InputBuilder builder) {
    this.source = builder.source;
    this.data = builder.data;

  }

  public static Input.InputBuilder builder() {
    return new Input.InputBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class InputBuilder {

    private Source source;

    private Data data;

    public Input.InputBuilder source(Source source) {
      this.source = source;
      return this;
    }

    public Input.InputBuilder data(Data data) {
      this.data = data;
      return this;
    }

    public Input build() {
      Input input = new Input(this);
      return input;
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
    Input input = (Input) o;
    return Objects.equals(this.source, input.source) && Objects.equals(this.data, input.data);
  }

  @Override
  public int hashCode() {
    return Objects.hash(source, data);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Input{");
    sb.append(" source:").append(source).append(",");
    sb.append(" data:").append(data);
    sb.append("}");
    return sb.toString();
  }


}
