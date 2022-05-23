package com.corunet.scsplugin.business_model.model.event.consumer;

import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.corunet.scsplugin.business_model.model.event.CreateOrder;

@Configuration
public class Subscriber {

    private final ISubscribeOperation subscribeOperation;

    protected Subscriber(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrder> subscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }


}
