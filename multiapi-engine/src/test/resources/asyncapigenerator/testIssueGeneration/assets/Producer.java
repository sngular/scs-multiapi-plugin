package net.coru.scsplugin.issuegeneration.model.event.producer;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import net.coru.scsplugin.issuegeneration.model.event.DataClientDTO;

@Configuration
public class Producer {

  private final IClients clients;

  protected Producer(final IClients clients) {
    this.clients = clients;
  }

  @Bean
  public Supplier<DataClientDTO> clients() {
    return () -> clients.clients();
  }


}
