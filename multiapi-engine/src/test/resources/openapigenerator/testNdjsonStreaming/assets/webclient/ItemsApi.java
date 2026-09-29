package com.sngular.multifileplugin.ndjsonstreamingwebclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.multifileplugin.ndjsonstreamingwebclient.client.ApiWebClient;
import com.sngular.multifileplugin.ndjsonstreamingwebclient.model.ItemDTO;
import com.sngular.multifileplugin.ndjsonstreamingwebclient.model.ErrorDTO;
import com.sngular.multifileplugin.ndjsonstreamingwebclient.model.ImportResultDTO;

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
   * GET /items: ""
   * @param filter  false
   * @return One item per line; (status code 200) The filter is not valid; (status code 400)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec listItemsRequestCreation(String filter) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiWebClient.parameterToMultiValueMap( null, "filter", filter));

    final String[] localVarAccepts = {"application/x-ndjson"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /items
   * @param filter  
   * @return One item per line; (status code 200) The filter is not valid; (status code 400)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ItemDTO> listItems(String filter) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return listItemsRequestCreation(filter).bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<Flux<ItemDTO>>> listItemsWithHttpInfo(String filter) throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return listItemsRequestCreation(filter).toEntityFlux(localVarReturnType);
  }

  /**
   * GET /items/lines: ""
   * @return One item per line, as JSON Lines; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec listItemLinesRequestCreation() throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/jsonl"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items/lines", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /items/lines
   * @return One item per line, as JSON Lines; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ItemDTO> listItemLines() throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return listItemLinesRequestCreation().bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<Flux<ItemDTO>>> listItemLinesWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return listItemLinesRequestCreation().toEntityFlux(localVarReturnType);
  }

  /**
   * POST /items/lines: ""
   * @param itemDTO (required)
   * @return Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec importItemLinesRequestCreation(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    Object postBody = itemDTO;
    if (itemDTO == null) {
    throw new WebClientResponseException("Missing the required parameter ''itemDTO'' when calling importItemLines", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/jsonl"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items/lines", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /items/lines
   * @param itemDTO   (required)
   * @return Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<ImportResultDTO> importItemLines(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return importItemLinesRequestCreation(itemDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<ImportResultDTO>> importItemLinesWithHttpInfo(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return importItemLinesRequestCreation(itemDTO).toEntity(localVarReturnType);
  }

  /**
   * GET /items/export: ""
   * @return The items, as a JSON array or one per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec exportItemsRequestCreation() throws WebClientResponseException {
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

    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items/export", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /items/export
   * @return The items, as a JSON array or one per line; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ItemDTO> exportItems() throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return exportItemsRequestCreation().bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<Flux<ItemDTO>>> exportItemsWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<ItemDTO> localVarReturnType = new ParameterizedTypeReference<ItemDTO>() {};
    return exportItemsRequestCreation().toEntityFlux(localVarReturnType);
  }

  /**
   * POST /items/import: ""
   * @param itemDTO (required)
   * @return Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec importItemsRequestCreation(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    Object postBody = itemDTO;
    if (itemDTO == null) {
    throw new WebClientResponseException("Missing the required parameter ''itemDTO'' when calling importItems", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/x-ndjson"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/items/import", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /items/import
   * @param itemDTO   (required)
   * @return Import summary; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<ImportResultDTO> importItems(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return importItemsRequestCreation(itemDTO).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<ImportResultDTO>> importItemsWithHttpInfo(Flux<ItemDTO> itemDTO) throws WebClientResponseException {
    ParameterizedTypeReference<ImportResultDTO> localVarReturnType = new ParameterizedTypeReference<ImportResultDTO>() {};
    return importItemsRequestCreation(itemDTO).toEntity(localVarReturnType);
  }

  /**
   * GET /items/{itemId}: ""
   * @param itemId  true
   * @return The item; (status code 200)
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
   * @return The item; (status code 200)
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

}