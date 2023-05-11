package com.sngular.apigenerator.asyncapi.model.schemas;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = TestMsgPayload.TestMsgPayloadBuilder.class)
public class TestMsgPayload {

  @JsonProperty(value ="id")
  private Integer id;

  private TestMsgPayload(Integer id) {
    this.id = id;

  }

  private TestMsgPayload(TestMsgPayloadBuilder builder) {
    this.id = builder.id;

  }

  public static TestMsgPayload.TestMsgPayloadBuilder builder() {
    return new TestMsgPayload.TestMsgPayloadBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TestMsgPayloadBuilder {

    private Integer id;

    public TestMsgPayload.TestMsgPayloadBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public TestMsgPayload build() {
      TestMsgPayload testMsgPayload = new TestMsgPayload(this);
      return testMsgPayload;
    }
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = false)
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
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
    TestMsgPayload testMsgPayload = (TestMsgPayload) o;
    return Objects.equals(this.id, testMsgPayload.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestMsgPayload {\n");
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
