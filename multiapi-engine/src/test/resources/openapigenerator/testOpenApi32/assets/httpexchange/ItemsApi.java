package com.sngular.multifileplugin.openapi32httpexchange;

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

import com.sngular.multifileplugin.openapi32httpexchange.model.FilterDTO;
import com.sngular.multifileplugin.openapi32httpexchange.model.ItemDTO;

/**
 * Items API, as a Spring HTTP service interface. Back it with a configured {@code RestClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(RestClientAdapter.create(restClient)).build()
 *     .createClient(ItemsApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface ItemsApi {

  /**
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO  (required)
   * @return The matching items (status code 200);
   */
  @HttpExchange(method = "QUERY", url = "/items", accept = {"application/json"}, contentType = "application/json")
  ResponseEntity<List<ItemDTO>> queryItems(@RequestBody(required = true) FilterDTO filterDTO);

  /**
   * OPTIONS /items
   * @return The methods allowed (status code 204);
   */
  @HttpExchange(method = "OPTIONS", url = "/items")
  ResponseEntity<Void> itemsOptions();

  /**
   * GET /items/search
   * @param name 
   * @param page 
   * @return The items found (status code 200);
   */
  @GetExchange(url = "/items/search", accept = {"application/json"})
  ResponseEntity<List<ItemDTO>> searchItems(@RequestParam(name = "name", required = false) String name, @RequestParam(name = "page", required = false) Integer page);

  /**
   * GET /items/{itemId}
   * @param itemId  (required)
   * @return The item (status code 200); No such item (status code 404);
   */
  @GetExchange(url = "/items/{itemId}", accept = {"application/json"})
  ResponseEntity<ItemDTO> getItem(@PathVariable("itemId") String itemId);

  /**
   * HEAD /items/{itemId}
   * @param itemId  (required)
   * @return The item exists (status code 200);
   */
  @HttpExchange(method = "HEAD", url = "/items/{itemId}")
  ResponseEntity<Void> checkItem(@PathVariable("itemId") String itemId);

  /**
   * PURGE /items/{itemId}
   * @param itemId  (required)
   * @return Purged from the caches (status code 204);
   */
  @HttpExchange(method = "PURGE", url = "/items/{itemId}")
  ResponseEntity<Void> purgeItem(@PathVariable("itemId") String itemId);
}
