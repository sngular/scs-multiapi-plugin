package com.corunet;

import java.util.function.Supplier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.corunet.apigenerator.asyncapi.model.OrderCreated;

@Configuration
public class Producer {

    private final IPublishOperation publishOperation;

    protected Producer(final IPublishOperation publishOperation){
      this.publishOperation = publishOperation;
    }

    @Bean
    public Supplier<OrderCreated> publishOperation(){ return () -> publishOperation.publishOperation(); }


}
