package com.sngular.scsplugin.arraywithstring.supplier;

import java.util.function.Supplier;

import com.sngular.scsplugin.arraywithstring.asyncapi.model.schemas.ObjectArrayDTO;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Producer {

  private final IObjectArrayString objectArrayString;

  protected Producer(final IObjectArrayString objectArrayString) {
    this.objectArrayString = objectArrayString;
  }

  @Bean
  public Supplier<ObjectArrayDTO> objectArrayString() {
    return () -> objectArrayString.objectArrayString();
  }


}
