package net.coru.api.generator.multiapi.integration.test.ScsMultiapiGenerationTest.testScsMultiapiGeneration.assets;

import java.util.function.Supplier;

import net.coru.scsplugin.business_model.model.event.CreateOrderMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

    private final ISubscribeOperation subscribeOperation;

    protected Producer(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Supplier<CreateOrderMapper> subscribeOperation(){ return () -> subscribeOperation.subscribeOperation(); }


}
