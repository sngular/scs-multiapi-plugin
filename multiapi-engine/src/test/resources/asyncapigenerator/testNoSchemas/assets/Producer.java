package com.sngular.scsplugin.noschemas;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.noschemas.model.messages.TestMsg;
import com.sngular.scsplugin.noschemas.model.schemas.Thing;

@Configuration
public class Producer {

  private final IOnTest onTest;

  private final IOnTest2 onTest2;

  protected Producer(final IOnTest onTest, final IOnTest2 onTest2) {
    this.onTest = onTest;
    this.onTest2 = onTest2;
  }

  @Bean
  public Supplier<TestMsg> onTest() {
    return () -> onTest.onTest();
  }

  @Bean
  public Supplier<Thing> onTest2() {
    return () -> onTest2.onTest2();
  }


}
