package com.sngular.api.generator.multiapi.integration.test.ScsMultiapiGenerationTest.testMultiapiGeneratedSourcesFolder.assets;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.generator.multiapi.model.event.OrderCreatedDTO;

@Configuration
public class TestClassName {

  private final IPublishOperation publishOperation;

  protected TestClassName(final IPublishOperation publishOperation) {
    this.publishOperation = publishOperation;
  }

  @Bean
  public Consumer<OrderCreatedDTO> publishOperation() {
    return value -> publishOperation.publishOperation(value);
  }


}
