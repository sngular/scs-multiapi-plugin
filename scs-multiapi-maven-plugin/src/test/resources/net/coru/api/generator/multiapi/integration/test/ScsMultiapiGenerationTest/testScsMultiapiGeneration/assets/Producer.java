package net.coru.generator.multiapi.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import net.coru.generator.multiapi.model.event.CreateOrderMapper;

@Configuration
public class Producer {

  private final ISubscribeOperation subscribeOperation;

  protected Producer(final ISubscribeOperation subscribeOperation) {
    this.subscribeOperation = subscribeOperation;
  }

  @Bean
  public Supplier<CreateOrderMapper> subscribeOperation() {
    return () -> subscribeOperation.subscribeOperation();
  }


}
