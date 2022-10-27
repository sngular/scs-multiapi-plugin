package net.coru.scsplugin.filegeneration.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import net.coru.scsplugin.filegeneration.model.event.CreateOrderMapper;

@Configuration
public class Producer {

  private final ISubscribeOperationFileGeneration subscribeOperationFileGeneration;

  protected Producer(final ISubscribeOperationFileGeneration subscribeOperationFileGeneration) {
    this.subscribeOperationFileGeneration = subscribeOperationFileGeneration;
  }

  @Bean
  public Supplier<CreateOrderMapper> subscribeOperationFileGeneration() {
    return () -> subscribeOperationFileGeneration.subscribeOperationFileGeneration();
  }


}
