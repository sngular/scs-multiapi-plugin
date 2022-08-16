package net.coru.api.generator.multiapi.integration.test.ScsMultiapiGenerationTest.testMultiapiGeneratedSourcesFolder.assets;

import java.util.function.Consumer;

import net.coru.scsplugin.business_model.model.event.OrderCreatedDTO;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestClassName {

    private final IPublishOperation publishOperation;

    protected TestClassName(final IPublishOperation publishOperation){
      this.publishOperation = publishOperation;
    }

    @Bean
    public Consumer<OrderCreatedDTO> publishOperation(){ return value -> publishOperation.publishOperation(value); }


}
