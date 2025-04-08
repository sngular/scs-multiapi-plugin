package com.sngular.scsplugin.messagenaming;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.messagenaming.model.TestMsg;
import com.sngular.scsplugin.messagenaming.model.TestMsg2;
import com.sngular.scsplugin.messagenaming.model.TestMsg3;

@Configuration
public class Producer {

  private final IOnTest onTest;

  private final IOnTest2 onTest2;

  private final IOnTest3 onTest3;

  protected Producer(final IOnTest onTest, final IOnTest2 onTest2, final IOnTest3 onTest3) {
    this.onTest = onTest;
    this.onTest2 = onTest2;
    this.onTest3 = onTest3;
  }

  @Bean
  public Supplier<TestMsg> onTest() {
    return () -> onTest.onTest();
  }

  @Bean
  public Supplier<TestMsg2> onTest2() {
    return () -> onTest2.onTest2();
  }

  @Bean
  public Supplier<TestMsg3> onTest3() {
    return () -> onTest3.onTest3();
  }


}
