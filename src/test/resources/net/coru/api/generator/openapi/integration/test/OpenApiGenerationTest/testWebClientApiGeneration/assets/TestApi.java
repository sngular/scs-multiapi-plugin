package net.coru.multifileplugin.testwebclient;

import net.coru.apigenerator.openapi.client.ApiWebClient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.coru.multifileplugin.testwebclient.model.ApiTestInfoDTO;
import net.coru.multifileplugin.testwebclient.model.ApiErrorDTO;
import net.coru.multifileplugin.testwebclient.model.ApiTestDTO;

import net.coru.apigenerator.openapi.client.auth.Authentication;
import net.coru.apigenerator.openapi.client.auth.HttpBasicAuth;

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

  public TestApi() {
    this.init();
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.authenticationsApi.put("BasicAuth", new HttpBasicAuth());
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  /**
  * GET /test/{testId} : Info for a specific test
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
    return apiWebClient.invokeAPI("http://localhost:8080/v1","/test/{testId}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
  * GET /test/{testId} : Info for a specific test
  * @param testId The id of the test to retrieve true
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

  /**
  * GET /test : List all available test
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

    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return apiWebClient.invokeAPI("http://localhost:8080/v1","/test", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
  * GET /test : List all available test
  * @return A paged array of tests; (status code 200)
  * @throws WebClientResponseException if an error occurs while attempting to invoke the API
  */
  public Mono<ApiTestDTO> listTest() throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return listTestRequestCreation().bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<ApiTestDTO>> listTestWithHttpInfo() throws WebClientResponseException {
    ParameterizedTypeReference<ApiTestDTO> localVarReturnType = new ParameterizedTypeReference<ApiTestDTO>() {};
    return listTestRequestCreation().toEntity(localVarReturnType);
  }


}