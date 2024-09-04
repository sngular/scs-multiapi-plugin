package com.sngular.multifileplugin.clpkgwebclientapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import com.sngular.multifileplugin.clpkgwebclientapi.model.exception.ModelClassException;

public class TestDTO {

  @JsonProperty(value ="name")
  private final String name;
  @JsonProperty(value ="id")
  private final Integer id;

  private TestDTO(TestDTOBuilder builder) {
    this.name = builder.name;
    this.id = builder.id;

    validateRequiredAttributes();
  }

  public static TestDTO.TestDTOBuilder builder() {
    return new TestDTO.TestDTOBuilder();
  }

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


  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }


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
    sb.append("TestDTO{");
    sb.append(" name:").append(name).append(",");
    sb.append(" id:").append(id);
    sb.append("}");
    return sb.toString();
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
