package groupId;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.apigenerator.asyncapi.model.schemas.CreateOrder;

@Configuration
public class Subscriber {

  private final ISubscribeOperationFileGenerationNoConf subscribeOperationFileGenerationNoConf;

  protected Subscriber(final ISubscribeOperationFileGenerationNoConf subscribeOperationFileGenerationNoConf) {
    this.subscribeOperationFileGenerationNoConf = subscribeOperationFileGenerationNoConf;
  }

  @Bean
  public Consumer<CreateOrder> subscribeOperationFileGenerationNoConf() {
    return value -> subscribeOperationFileGenerationNoConf.subscribeOperationFileGenerationNoConf(value);
  }


}