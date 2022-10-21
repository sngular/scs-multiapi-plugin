package net.coru.scsplugin.filegenerationissue.model.event.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import net.coru.scsplugin.filegenerationissue.model.event.CustomerEventMessageDTO;

@Configuration
public class TestClassName {

  private final IOnCustomerEvent onCustomerEvent;

  protected TestClassName(final IOnCustomerEvent onCustomerEvent) {
    this.onCustomerEvent = onCustomerEvent;
  }

  @Bean
  public Consumer<CustomerEventMessageDTO> onCustomerEvent() {
    return value -> onCustomerEvent.onCustomerEvent(value);
  }


}
