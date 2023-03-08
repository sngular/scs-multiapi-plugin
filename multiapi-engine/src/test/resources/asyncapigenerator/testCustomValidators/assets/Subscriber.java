package com.sngular.scsplugin.customvalidators.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.customvalidators.model.event.StatusDTO;

@Configuration
public class Subscriber {

  private final ICustomValidatorResponse customValidatorResponse;

  protected Subscriber(final ICustomValidatorResponse customValidatorResponse) {
    this.customValidatorResponse = customValidatorResponse;
  }

  @Bean
  public Consumer<StatusDTO> customValidatorResponse() {
    return value -> customValidatorResponse.customValidatorResponse(value);
  }


}
