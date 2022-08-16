package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationTwoYmlFiles.assets;

import java.util.function.Supplier;

import net.coru.apigenerator.asyncapi.model.OrderCreated;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

    private final IPublishOperation publishOperation;

    protected Producer(final IPublishOperation publishOperation){
      this.publishOperation = publishOperation;
    }

    @Bean
    public Supplier<OrderCreated> publishOperation(){ return () -> publishOperation.publishOperation(); }


}
