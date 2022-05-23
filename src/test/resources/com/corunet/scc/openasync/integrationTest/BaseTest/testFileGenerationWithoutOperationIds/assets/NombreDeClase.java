package com.corunet.scsplugin.business_model.model.event.consumer;

import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.corunet.scsplugin.business_model.model.event.CreateOrderDTO;

@Configuration
public class NombreDeClase {

    private final ISubscribeOperation subscribeOperation;

    protected NombreDeClase(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrderDTO> subscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }


}
