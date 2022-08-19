package net.coru.scsplugin.withOutIds.model.event.consumer;

import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import net.coru.scsplugin.withOutIds.model.event.CreateOrderDTO;

@Configuration
public class TestClassName {

    private final ISubscribeOperation subscribeOperation;

    protected TestClassName(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrderDTO> subscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }


}
