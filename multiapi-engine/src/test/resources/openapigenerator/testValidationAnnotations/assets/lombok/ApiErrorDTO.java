package com.sngular.multifileplugin.lombok.testapi.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.ArrayList;
import com.sngular.multifileplugin.testapi.model.customvalidator.Size;
import com.sngular.multifileplugin.testapi.model.customvalidator.Max;
import com.sngular.multifileplugin.testapi.model.customvalidator.Min;
import com.sngular.multifileplugin.testapi.model.customvalidator.Pattern;
import com.sngular.multifileplugin.testapi.model.customvalidator.MultipleOf;
import com.sngular.multifileplugin.testapi.model.customvalidator.MaxItems;
import com.sngular.multifileplugin.testapi.model.customvalidator.MinItems;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import lombok.extern.jackson.Jacksonized;

@Data
public class ApiErrorDTO {

  @JsonProperty(value ="code")
  @Min(minimum = 10)
  @Max(maximum = 200, exclusive = true)
  @MultipleOf(multiple = "10.55")
  @NonNull
  private Integer code;

  @JsonProperty(value ="message")
  @Size(min =50, max =200)
  @Pattern(regex = "^[a-zA-Z0-9_.-]*$")
  @NonNull
  private String message;

  @JsonProperty(value ="test")
  @MaxItems(maximum = 10)
  @MinItems(minimum = 5)
  private List<Integer> test = new ArrayList<Integer>();


  @Builder
  @Jacksonized
  private ApiErrorDTO(@NonNull Integer code, @NonNull String message, List<Integer> test) {
    this.code = code;
    this.message = message;
    this.test = test;

  }

}
