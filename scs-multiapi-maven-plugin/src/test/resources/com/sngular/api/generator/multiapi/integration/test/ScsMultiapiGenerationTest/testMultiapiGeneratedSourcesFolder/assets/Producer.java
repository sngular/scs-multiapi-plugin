package com.sngular.api.generator.multiapi.integration.test.ScsMultiapiGenerationTest.testMultiapiGeneratedSourcesFolder.assets;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.generator.multiapi.model.event.CreateOrderMapper;

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
