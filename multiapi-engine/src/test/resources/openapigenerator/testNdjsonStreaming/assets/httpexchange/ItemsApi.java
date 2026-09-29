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

import com.sngular.multifileplugin.ndjsonstreaminghttpexchange.model.ItemDTO;
import com.sngular.multifileplugin.ndjsonstreaminghttpexchange.model.ErrorDTO;
import com.sngular.multifileplugin.ndjsonstreaminghttpexchange.model.ImportResultDTO;

/**
 * Items API, as a Spring HTTP service interface. Back it with a configured {@code WebClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(WebClientAdapter.create(webClient)).build()
 *     .createClient(ItemsApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface ItemsApi {

  /**
   * GET /items
   * @param filter 
   * @return One item per line (status code 200); The filter is not valid (status code 400);
   */
  @GetExchange(url = "/items", accept = {"application/x-ndjson"})
  Mono<ResponseEntity<Flux<ItemDTO>>> listItems(@RequestParam(name = "filter", required = false) String filter);

  /**
   * GET /items/lines
   * @return One item per line, as JSON Lines (status code 200);
   */
  @GetExchange(url = "/items/lines", accept = {"application/jsonl"})
  Mono<ResponseEntity<Flux<ItemDTO>>> listItemLines();

  /**
   * POST /items/lines
   * @param itemDTO  (required)
   * @return Import summary (status code 200);
   */
  @PostExchange(url = "/items/lines", accept = {"application/json"}, contentType = "application/jsonl")
  Mono<ResponseEntity<ImportResultDTO>> importItemLines(@RequestBody(required = true) Flux<ItemDTO> itemDTO);

  /**
   * GET /items/export
   * @return The items, as a JSON array or one per line (status code 200);
   */
  @GetExchange(url = "/items/export", accept = {"application/x-ndjson"})
  Mono<ResponseEntity<Flux<ItemDTO>>> exportItems();

  /**
   * POST /items/import
   * @param itemDTO  (required)
   * @return Import summary (status code 200);
   */
  @PostExchange(url = "/items/import", accept = {"application/json"}, contentType = "application/x-ndjson")
  Mono<ResponseEntity<ImportResultDTO>> importItems(@RequestBody(required = true) Flux<ItemDTO> itemDTO);

  /**
   * GET /items/{itemId}
   * @param itemId  (required)
   * @return The item (status code 200);
   */
  @GetExchange(url = "/items/{itemId}", accept = {"application/json"})
  Mono<ResponseEntity<ItemDTO>> getItem(@PathVariable("itemId") String itemId);
}
