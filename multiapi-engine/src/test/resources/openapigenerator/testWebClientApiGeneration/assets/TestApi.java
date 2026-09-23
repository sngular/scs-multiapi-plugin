package com.sngular.multifileplugin.webclientapi;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.sngular.apigenerator.openapi.client.ApiWebClient;
import com.sngular.multifileplugin.webclientapi.model.ApiTestDTO;
import com.sngular.multifileplugin.webclientapi.model.ApiErrorDTO;
import com.sngular.multifileplugin.webclientapi.model.ApiTestInfoDTO;

import com.sngular.apigenerator.openapi.client.auth.Authentication;
import com.sngular.apigenerator.openapi.client.auth.HttpBasicAuth;

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

public class TestApi {

  private ApiWebClient apiWebClient;

  private Map<String, Authentication> authenticationsApi;

  private String basePath = "http://localhost:8080/v1";

  /**
   * Builds its own ApiWebClient, sending requests to the contract's first server.
   */
  public TestApi() {
    this.init();
  }

  /**
   * Sends requests through the given client, e.g. one built on the service's configured WebClient, to the contract's first server.
   */
  public TestApi(final ApiWebClient apiWebClient) {
    this.apiWebClient = Objects.requireNonNull(apiWebClient, "apiWebClient");
  }

  /**
   * Sends requests through the given client to {@code basePath}. An empty base path sends them relative to the client's own
   * base URL (WebClient.Builder.baseUrl(...)).
   */
  public TestApi(final ApiWebClient apiWebClient, final String basePath) {
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
    this.authenticationsApi.put("BasicAuth", new HttpBasicAuth());
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  /**
   * GET /test: List all available test
   * @return A paged array of tests; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec listTestRequestCreation() throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"BasicAuth"};

    ParameterizedTypeReference<List<ApiTestDTO>> localVarReturnType = new ParameterizedTypeReference<List<ApiTestDTO>>() {};
    return apiWebClient.invokeAPI(basePath,"/test", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /test: List all available test
   * @return A paged array of tests; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Flux<ApiTestDTO> listTest() throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return listTestRequestCreation().bodyToFlux(localVarReturnType);
  }

  public Mono<ResponseEntity<List<ApiTestDTO>>> listTestWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return listTestRequestCreation().toEntityList(localVarReturnType);
  }

  /**
   * GET /test/{testId}: Info for a specific test
   * @param testId The id of the test to retrieve true
   * @return Expected response to a valid request; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec showTestByIdRequestCreation(Integer testId) throws WebClientResponseException {
    Object postBody = null;
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("testId",  testId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {"BasicAuth"};

    ParameterizedTypeReference<ApiTestInfoDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestInfoDTO>() {};
    return apiWebClient.invokeAPI(basePath,"/test/{testId}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * GET /test/{testId}: Info for a specific test
   * @param testId The id of the test to retrieve (required)
   * @return Expected response to a valid request; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<ApiTestInfoDTO> showTestById(Integer testId) throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestInfoDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestInfoDTO>() {};
    return showTestByIdRequestCreation(testId).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<ApiTestInfoDTO>> showTestByIdWithHttpInfo(Integer testId) throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestInfoDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestInfoDTO>() {};
    return showTestByIdRequestCreation(testId).toEntity(localVarReturnType);
  }

}