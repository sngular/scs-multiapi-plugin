package com.sngular.multifileplugin.pathparameter.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.pathparameter.model.exception.ModelClassException;
import com.sngular.multifileplugin.pathparameter.model.customvalidator.NotNull;

@JsonDeserialize(builder = TestDTO.TestDTOBuilder.class)
public class TestDTO {

  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="id")
  @NotNull
  private final Integer id;

  private TestDTO(String name, Integer id) {
    this.name = name;
    this.id = id;

    validateRequiredAttributes();
  }

  private TestDTO(TestDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

    validateRequiredAttributes();
  }

  public static TestDTO.TestDTOBuilder builder() {
    return new TestDTO.TestDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class TestDTOBuilder {

    private String name;
    private Integer id;

    public TestDTO.TestDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public TestDTO.TestDTOBuilder id(Integer id) {
      this.id = id;
      return this;
    }

    public TestDTO build() {
      TestDTO testDTO = new TestDTO(this);
      return testDTO;
    }
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = true)
  public Integer getId() {
    return id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TestDTO testDTO = (TestDTO) o;
    return Objects.equals(this.name, testDTO.name) && Objects.equals(this.id, testDTO.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestDTO {\n");
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


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.id)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("TestDTO");
    }
  }

}
