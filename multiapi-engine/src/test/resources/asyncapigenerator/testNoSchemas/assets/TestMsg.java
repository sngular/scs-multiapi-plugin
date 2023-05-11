package com.sngular.apigenerator.asyncapi.model.messages;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@JsonDeserialize(builder = TestMsg.TestMsgBuilder.class)
public class TestMsg {

  @JsonProperty(value ="payload")
  private com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload payload;

  private TestMsg(com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload payload) {
    this.payload = payload;

  }

  private TestMsg(TestMsgBuilder builder) {
    this.payload = builder.payload;

  }

  public static TestMsg.TestMsgBuilder builder() {
    return new TestMsg.TestMsgBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TestMsgBuilder {

    private com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload payload;

    public TestMsg.TestMsgBuilder payload(com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload payload) {
      this.payload = payload;
      return this;
    }

    public TestMsg build() {
      TestMsg testMsg = new TestMsg(this);
      return testMsg;
    }
  }

  /**
  * Get payload
  * @return payload
  */
  @Schema(name = "payload", required = false)
  public com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload getPayload() {
    return payload;
  }
  public void setPayload(com.sngular.apigenerator.asyncapi.model.schemas.TestMsgPayload payload) {
    this.payload = payload;
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
    return Objects.equals(this.payload, testMsg.payload);
  }

  @Override
  public int hashCode() {
    return Objects.hash(payload);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestMsg {\n");
    sb.append(" payload: ").append(toIndentedString(payload)).append("\n");
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
