package com.sngular.scsplugin.externalavro.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.testshop.commons.Receipt;
import com.sngular.scsplugin.externalavro.model.event.CreateOrder;

@Configuration
public class Subscriber {

  private final ISubscribeReceiptExternalAvro subscribeReceiptExternalAvro;

  private final ISubscribeOperationExternalAvro subscribeOperationExternalAvro;

  protected Subscriber(final ISubscribeReceiptExternalAvro subscribeReceiptExternalAvro, final ISubscribeOperationExternalAvro subscribeOperationExternalAvro) {
    this.subscribeReceiptExternalAvro = subscribeReceiptExternalAvro;
    this.subscribeOperationExternalAvro = subscribeOperationExternalAvro;
  }

  @Bean
  public Consumer<Receipt> subscribeReceiptExternalAvro() {
    return value -> subscribeReceiptExternalAvro.subscribeReceiptExternalAvro(value);
  }

  @Bean
  public Consumer<CreateOrder> subscribeOperationExternalAvro() {
    return value -> subscribeOperationExternalAvro.subscribeOperationExternalAvro(value);
  }


}
