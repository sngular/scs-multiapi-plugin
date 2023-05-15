package com.sngular.scsplugin.noschemas.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = TestMsg.TestMsgBuilder.class)
public class TestMsg {

  @JsonProperty(value ="id")
  private Integer id;

  private TestMsg(Integer id) {
    this.id = id;

  }

  private TestMsg(TestMsgBuilder builder) {
    this.id = builder.id;

  }

  public static TestMsg.TestMsgBuilder builder() {
    return new TestMsg.TestMsgBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TestMsgBuilder {

    private Integer id;

    public TestMsg.TestMsgBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public TestMsg build() {
      TestMsg testMsg = new TestMsg(this);
      return testMsg;
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
    TestMsg testMsg = (TestMsg) o;
    return Objects.equals(this.id, testMsg.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestMsg {\n");
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
