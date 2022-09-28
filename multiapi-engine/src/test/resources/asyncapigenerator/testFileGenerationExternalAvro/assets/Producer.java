package net.coru.scsplugin.externalavro.model.event.producer;

import java.util.function.Supplier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import net.coru.scsplugin.externalavro.model.event.Order;

@Configuration
public class Producer {

    private final IPublishOperationExternalAvro publishOperationExternalAvro;

    protected Producer(final IPublishOperationExternalAvro publishOperationExternalAvro){
      this.publishOperationExternalAvro = publishOperationExternalAvro;
    }

    @Bean
    public Supplier<Order> publishOperationExternalAvro(){ return () -> publishOperationExternalAvro.publishOperationExternalAvro(); }


}
