package com.sngular.multifileplugin.ndjsonstreamingwebclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.ndjsonstreamingwebclient.client.ApiWebClient;
import com.sngular.multifileplugin.ndjsonstreamingwebclient.model.InlineResponse200StreamEventsDTO;

import com.sngular.multifileplugin.ndjsonstreamingwebclient.client.auth.Authentication;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class EventsApi {

  private ApiWebClient apiWebClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiWebClient, sending requests to the contract's first server.
   */
  public EventsApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured WebClient, to the contract's first server.
   */
  public EventsApi(final ApiWebClient apiWebClient) {
    this.apiWebClient = Objects.requireNonNull(apiWebClient, "apiWebClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * base URL (WebClient.Builder.baseUrl(...)).
   */
  public EventsApi(final ApiWebClient apiWebClient, final String basePath) {
    this(apiWebClient);
    this.basePath = basePath;
  }

  public String getBasePath() {
    return basePath;
  }

  public void setBasePath(final String basePath) {
    this.basePath = basePath;
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  /**
   * GET /events: ""
   * @return One event per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec streamEventsRequestCreation() throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/x-ndjson"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<InlineResponse200StreamEventsDTO> localVarReturnType = new ParameterizedTypeReference<InlineResponse200StreamEventsDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/events", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /events
   * @return One event per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<InlineResponse200StreamEventsDTO> streamEvents() throws WebClientResponseException {
    ParameterizedTypeReference<InlineResponse200StreamEventsDTO> localVarReturnType = new ParameterizedTypeReference<InlineResponse200StreamEventsDTO>() {};
    return streamEventsRequestCreation().bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<Flux<InlineResponse200StreamEventsDTO>>> streamEventsWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<InlineResponse200StreamEventsDTO> localVarReturnType = new ParameterizedTypeReference<InlineResponse200StreamEventsDTO>() {};
    return streamEventsRequestCreation().toEntityFlux(localVarReturnType);
  }

}