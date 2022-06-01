package net.coru.scsplugin.business_model.model.event.producer;

import java.util.function.Supplier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import net.coru.scsplugin.business_model.model.event.Order;

@Configuration
public class Producer {

    private final IPublishOperation publishOperation;

    protected Producer(final IPublishOperation publishOperation){
      this.publishOperation = publishOperation;
    }

    @Bean
    public Supplier<Order> publishOperation(){ return () -> publishOperation.publishOperation(); }


}
