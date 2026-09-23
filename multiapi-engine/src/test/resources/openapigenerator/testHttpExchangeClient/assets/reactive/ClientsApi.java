package com.sngular.multifileplugin.httpexchangereactive;

import java.time.LocalDate;
import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.format.annotation.DateTimeFormat;
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
import reactor.core.publisher.Mono;

import com.sngular.multifileplugin.httpexchangereactive.model.ClientDTO;

/**
 * Clients API, as a Spring HTTP service interface. Back it with a configured {@code WebClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(WebClientAdapter.create(webClient)).build()
 *     .createClient(ClientsApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface ClientsApi {

  /**
   * GET /clients/{client_id}
   * @param client_id  (required)
   * @param since 
   * @param xCorrelationId 
   * @return The client (status code 200);
   */
  @GetExchange(url = "/clients/{client_id}", accept = {"application/json"})
  Mono<ResponseEntity<ClientDTO>> getClient(@PathVariable("client_id") Long client_id, @RequestParam(name = "since", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate since, @RequestHeader(name = "X-Correlation-Id", required = false) String xCorrelationId);

  /**
   * DELETE /clients/{client_id}
   * @param client_id  (required)
   * @return Deleted (status code 204);
   */
  @DeleteExchange(url = "/clients/{client_id}")
  Mono<ResponseEntity<Void>> deleteClient(@PathVariable("client_id") Long client_id);

  /**
   * GET /clients
   * @param page_num 
   * @return The clients (status code 200);
   */
  @GetExchange(url = "/clients", accept = {"application/json"})
  Mono<ResponseEntity<List<ClientDTO>>> searchClients(@RequestParam(name = "page_num", required = false, defaultValue = "0") Integer page_num);

  /**
   * POST /clients
   * @param clientDTO  (required)
   * @return Created (status code 201);
   */
  @PostExchange(url = "/clients", accept = {"application/json"}, contentType = "application/json")
  Mono<ResponseEntity<ClientDTO>> createClient(@RequestBody(required = true) ClientDTO clientDTO);

  /**
   * PUT /clients/{client_id}/document
   * @param client_id  (required)
   * @param file multipart part "file" (required)
   * @param comment multipart part "comment"
   * @return Uploaded (status code 204);
   */
  @PutExchange(url = "/clients/{client_id}/document", contentType = "multipart/form-data")
  Mono<ResponseEntity<Void>> uploadDocument(@PathVariable("client_id") Long client_id, @RequestPart(name = "file", required = true) MultipartFile file, @RequestPart(name = "comment", required = false) String comment);
}
