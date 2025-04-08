package com.sngular.scsplugin.streambridge.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.streambridge.model.event.CreateOrderMessageDTO;

@Configuration
public class TestClassName {

  private final ISubscribeOperationStreamBridge subscribeOperationStreamBridge;

  protected TestClassName(final ISubscribeOperationStreamBridge subscribeOperationStreamBridge) {
    this.subscribeOperationStreamBridge = subscribeOperationStreamBridge;
  }

  @Bean
  public Consumer<CreateOrderMessageDTO> subscribeOperationStreamBridge() {
    return value -> subscribeOperationStreamBridge.subscribeOperationStreamBridge(value);
  }


}
