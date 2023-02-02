package com.sngular.scsplugin.arraywithstring.supplier;

import java.util.function.Supplier;

import com.sngular.scsplugin.arraywithstring.asyncapi.model.ObjectArrayMessageDTO;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

  private final IObjectArrayString objectArrayString;

  protected Producer(final IObjectArrayString objectArrayString) {
    this.objectArrayString = objectArrayString;
  }

  @Bean
  public Supplier<ObjectArrayMessageDTO> objectArrayString() {
    return () -> objectArrayString.objectArrayString();
  }


}
