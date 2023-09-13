package com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer.model.StatusMsgDTO;

@Configuration
public class Subscriber {

  private final ICustomValidatorResponse customValidatorResponse;

  protected Subscriber(final ICustomValidatorResponse customValidatorResponse) {
    this.customValidatorResponse = customValidatorResponse;
  }

  @Bean
  public Consumer<StatusMsgDTO> customValidatorResponse() {
    return value -> customValidatorResponse.customValidatorResponse(value);
  }


}
