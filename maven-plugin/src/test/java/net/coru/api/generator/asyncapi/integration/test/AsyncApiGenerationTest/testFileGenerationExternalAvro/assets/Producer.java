package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationExternalAvro.assets;

import java.util.function.Supplier;

import net.coru.scsplugin.business_model.model.event.Order;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

    private final IPublishOperation publishOperation;

    protected Producer(final IPublishOperation publishOperation){
      this.publishOperation = publishOperation;
    }

    @Bean
    public Supplier<Order> publishOperation(){ return () -> publishOperation.publishOperation(); }


}
