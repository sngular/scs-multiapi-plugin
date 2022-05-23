package com.corunet.scsplugin.business_model.model.event.producer;

import java.util.function.Supplier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.corunet.scsplugin.business_model.model.event.CreateOrderMapper;

@Configuration
public class Producer {

    private final ISubscribeOperation subscribeOperation;

    protected Producer(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Supplier<CreateOrderMapper> subscribeOperation(){ return () -> subscribeOperation.subscribeOperation(); }


}
