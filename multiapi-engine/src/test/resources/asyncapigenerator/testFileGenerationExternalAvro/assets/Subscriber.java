package net.coru.scsplugin.externalavro.model.event.consumer;

import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import net.coru.scsplugin.externalavro.model.event.CreateOrder;

@Configuration
public class Subscriber {

    private final ISubscribeOperationExternalAvro subscribeOperationExternalAvro;

    protected Subscriber(final ISubscribeOperationExternalAvro subscribeOperationExternalAvro){
      this.subscribeOperationExternalAvro = subscribeOperationExternalAvro;
    }

    @Bean
    public Consumer<CreateOrder> subscribeOperationExternalAvro(){ return value -> subscribeOperationExternalAvro.subscribeOperationExternalAvro(value); }


}
