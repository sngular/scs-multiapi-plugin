package net.coru;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import net.coru.apigenerator.asyncapi.OrderCreated;

@Configuration
public class Producer {

  private final IPublishOperation publishOperation;

  protected Producer(final IPublishOperation publishOperation) {
    this.publishOperation = publishOperation;
  }

  @Bean
  public Supplier<OrderCreated> publishOperation() {
    return () -> publishOperation.publishOperation();
  }


}
