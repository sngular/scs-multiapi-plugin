package com.sngular.multifileplugin.openapi32webclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.openapi32webclient.client.ApiWebClient;
import com.sngular.multifileplugin.openapi32webclient.model.FilterDTO;
import com.sngular.multifileplugin.openapi32webclient.model.ItemDTO;
import com.sngular.multifileplugin.openapi32webclient.model.CriteriaDTO;

import com.sngular.multifileplugin.openapi32webclient.client.auth.Authentication;

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

public class ItemsApi {

  private ApiWebClient apiWebClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "";

  /**
   * Builds its own ApiWebClient, sending requests to the contract's first server.
   */
  public ItemsApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured WebClient, to the contract's first server.
   */
  public ItemsApi(final ApiWebClient apiWebClient) {
    this.apiWebClient = Objects.requireNonNull(apiWebClient, "apiWebClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * base URL (WebClient.Builder.baseUrl(...)).
   */
  public ItemsApi(final ApiWebClient apiWebClient, final String basePath) {
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
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO (required)
   * @return The matching items; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec queryItemsRequestCreation(FilterDTO filterDTO) throws WebClientResponseException {
    Object postBody = filterDTO;
    if (filterDTO == null) {
    throw new WebClientResponseException("Missing the required parameter ''filterDTO'' when calling queryItems", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<List<ItemDTO>> localVarReturnType = new ParameterizedTypeReference<List<ItemDTO>>() {};
    return apiWebClient.invokeAPI(basePath,"/items", HttpMethod.valueOf("QUERY"), pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * QUERY /items: Finds the items that match a filter
   * @param filterDTO   (required)
   * @return The matching items; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ItemDTO> queryItems(FilterDTO filterDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return queryItemsRequestCreation(filterDTO).bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<List<ItemDTO>>> queryItemsWithHttpInfo(FilterDTO filterDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return queryItemsRequestCreation(filterDTO).toEntityList(localVarReturnType);
  }

  /**
   * OPTIONS /items: ""
   * @return The methods allowed; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec itemsOptionsRequestCreation() throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/items", HttpMethod.OPTIONS, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * OPTIONS /items
   * @return The methods allowed; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> itemsOptions() throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return itemsOptionsRequestCreation().bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> itemsOptionsWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return itemsOptionsRequestCreation().toEntity(localVarReturnType);
  }

  /**
   * GET /items/search: ""
   * @param criteria  false
   * @return The items found; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec searchItemsRequestCreation(CriteriaDTO criteria) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiWebClient.objectToQueryParams("form", true, "criteria", criteria));

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<List<ItemDTO>> localVarReturnType = new ParameterizedTypeReference<List<ItemDTO>>() {};
    return apiWebClient.invokeAPI(basePath,"/items/search", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /items/search
   * @param criteria  
   * @return The items found; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ItemDTO> searchItems(CriteriaDTO criteria) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return searchItemsRequestCreation(criteria).bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<List<ItemDTO>>> searchItemsWithHttpInfo(CriteriaDTO criteria) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return searchItemsRequestCreation(criteria).toEntityList(localVarReturnType);
  }

  /**
   * GET /items/{itemId}: ""
   * @param itemId  true
   * @return The item; (status code 200) No such item; (status code 404)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec getItemRequestCreation(String itemId) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("itemId",  itemId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /items/{itemId}
   * @param itemId  (required)
   * @return The item; (status code 200) No such item; (status code 404)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<ItemDTO> getItem(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return getItemRequestCreation(itemId).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<ItemDTO>> getItemWithHttpInfo(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return getItemRequestCreation(itemId).toEntity(localVarReturnType);
  }

  /**
   * HEAD /items/{itemId}: ""
   * @param itemId  true
   * @return The item exists; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec checkItemRequestCreation(String itemId) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("itemId",  itemId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.HEAD, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * HEAD /items/{itemId}
   * @param itemId  (required)
   * @return The item exists; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> checkItem(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return checkItemRequestCreation(itemId).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> checkItemWithHttpInfo(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return checkItemRequestCreation(itemId).toEntity(localVarReturnType);
  }

  /**
   * PURGE /items/{itemId}: ""
   * @param itemId  true
   * @return Purged from the caches; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec purgeItemRequestCreation(String itemId) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("itemId",  itemId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return apiWebClient.invokeAPI(basePath,"/items/{itemId}", HttpMethod.valueOf("PURGE"), pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * PURGE /items/{itemId}
   * @param itemId  (required)
   * @return Purged from the caches; (status code 204)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Void> purgeItem(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return purgeItemRequestCreation(itemId).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Void>> purgeItemWithHttpInfo(String itemId) throws WebClientResponseException {
    ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
    return purgeItemRequestCreation(itemId).toEntity(localVarReturnType);
  }

}