package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationTwoYmlFiles.assets.producer2;

import java.util.function.Supplier;

import net.coru.scsplugin.business_model.model.event.OrderCreatedDTO;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

    private final IPublishOperation2 publishOperation2;

    protected Producer(final IPublishOperation2 publishOperation2){
      this.publishOperation2 = publishOperation2;
    }

    @Bean
    public Supplier<OrderCreatedDTO> publishOperation2(){ return () -> publishOperation2.publishOperation2(); }


}
