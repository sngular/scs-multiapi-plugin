package com.sngular.scsplugin.issuegeneration.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.issuegeneration.model.event.DataDTO;

@Configuration
public class Producer {

  private final IClients clients;

  protected Producer(final IClients clients) {
    this.clients = clients;
  }

  @Bean
  public Supplier<DataDTO> clients() {
    return () -> clients.clients();
  }


}
