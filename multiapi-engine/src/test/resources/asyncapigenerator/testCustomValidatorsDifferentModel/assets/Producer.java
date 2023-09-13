package com.sngular.scsplugin.customvalidatordifferentmodel.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.customvalidatordifferentmodel.event.producer.model.DataDTO;

@Configuration
public class Producer {

  private final ICustomValidatorClients customValidatorClients;

  protected Producer(final ICustomValidatorClients customValidatorClients) {
    this.customValidatorClients = customValidatorClients;
  }

  @Bean
  public Supplier<DataDTO> customValidatorClients() {
    return () -> customValidatorClients.customValidatorClients();
  }


}
