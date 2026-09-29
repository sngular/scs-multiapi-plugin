package com.sngular.multifileplugin.ndjsonstreaminghttpexchange;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PatchExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import com.sngular.multifileplugin.ndjsonstreaminghttpexchange.model.InlineResponse200StreamEventsDTO;

/**
 * Events API, as a Spring HTTP service interface. Back it with a configured {@code WebClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(WebClientAdapter.create(webClient)).build()
 *     .createClient(EventsApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface EventsApi {

  /**
   * GET /events
   * @return One event per line (status code 200);
   */
  @GetExchange(url = "/events", accept = {"application/x-ndjson"})
  Mono<ResponseEntity<Flux<InlineResponse200StreamEventsDTO>>> streamEvents();
}
