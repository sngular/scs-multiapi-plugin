package com.sngular.multifileplugin.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testapi.model.customvalidator.MaxInteger;
import com.sngular.multifileplugin.testapi.model.customvalidator.MinInteger;
import com.sngular.multifileplugin.testapi.model.exception.ModelClassException;
import com.sngular.multifileplugin.testapi.model.customvalidator.MultipleOf;
import com.sngular.multifileplugin.testapi.model.customvalidator.NotNull;
import com.sngular.multifileplugin.testapi.model.customvalidator.Size;
import com.sngular.multifileplugin.testapi.model.customvalidator.Pattern;
import com.sngular.multifileplugin.testapi.model.customvalidator.MaxItems;
import com.sngular.multifileplugin.testapi.model.customvalidator.MinItems;
import com.sngular.multifileplugin.testapi.model.customvalidator.UniqueItems;

@JsonDeserialize(builder = ApiErrorDTO.ApiErrorDTOBuilder.class)
public class ApiErrorDTO {

  @JsonProperty(value ="intCode")
  private Integer intCode;
  @JsonProperty(value ="code")
  @MinInteger(minimum = "10", exclusive = false)
  @MaxInteger(maximum = "200", exclusive = true)
  @MultipleOf(multiple = "10.55")
  @NotNull
  private final Integer code;
  @JsonProperty(value ="message")
  @Size(min =50, max =200)
  @Pattern(regex = "^[a-zA-Z0-9_.-]*$")
  @NotNull
  private final String message;
  @JsonProperty(value ="test")
  @MaxItems(maximum = 10)
  @MinItems(minimum = 5)
  @UniqueItems
  private List<Integer> test;

  private ApiErrorDTO(ApiErrorDTOBuilder builder) {
    this.intCode = builder.intCode;
    this.code = builder.code;
    this.message = builder.message;
    this.test = builder.test;

    validateRequiredAttributes();
  }

  public static ApiErrorDTO.ApiErrorDTOBuilder builder() {
    return new ApiErrorDTO.ApiErrorDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ApiErrorDTOBuilder {

    private Integer intCode;
    private Integer code;
    private String message;
    private List<Integer> test = new ArrayList<Integer>();

    public ApiErrorDTO.ApiErrorDTOBuilder intCode(Integer intCode) {
      this.intCode = intCode;
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder code(Integer code) {
      this.code = code;
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder message(String message) {
      this.message = message;
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder test(List<Integer> test) {
      if (!test.isEmpty()) {
        this.test.addAll(test);
      }
      return this;
    }

    public ApiErrorDTO.ApiErrorDTOBuilder _test(Integer _test) {
      if (Objects.nonNull(_test)) {
        this.test.add(_test);
      }
      return this;
    }

    public ApiErrorDTO build() {
      ApiErrorDTO apiErrorDTO = new ApiErrorDTO(this);
      return apiErrorDTO;
    }
  }

  @Schema(name = "intCode", required = false)
  public Integer getIntCode() {
    return intCode;
  }
  public void setIntCode(Integer intCode) {
    this.intCode = intCode;
  }

  @Schema(name = "code", required = true)
  public Integer getCode() {
    return code;
  }

  @Schema(name = "message", required = true)
  public String getMessage() {
    return message;
  }

  @Schema(name = "test", required = false)
  public List<Integer> getTest() {
    return test;
  }
  public void setTest(List<Integer> test) {
    this.test = test;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiErrorDTO apiErrorDTO = (ApiErrorDTO) o;
    return Objects.equals(this.intCode, apiErrorDTO.intCode) && Objects.equals(this.code, apiErrorDTO.code) && Objects.equals(this.message, apiErrorDTO.message) && Objects.equals(this.test, apiErrorDTO.test);
  }

  @Override
  public int hashCode() {
    return Objects.hash(intCode, code, message, test);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("ApiErrorDTO{");
    sb.append(" intCode:").append(intCode).append(",");
    sb.append(" code:").append(code).append(",");
    sb.append(" message:").append(message).append(",");
    sb.append(" test:").append(test);
    sb.append("}");
    return sb.toString();
  }

  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.code)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.message)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiErrorDTO");
    }
  }

}
