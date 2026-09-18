package com.sngular.multifileplugin.testrarecharsnameswebclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sngular.multifileplugin.testrarecharsnameswebclient.client.ApiWebClient;
import com.sngular.multifileplugin.testrarecharsnameswebclient.model.Shipment;

import com.sngular.multifileplugin.testrarecharsnameswebclient.client.auth.Authentication;

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

public class ShipmentApi {

  private ApiWebClient apiWebClient;

  private Map<String, Authentication> authenticationsApi;

  public ShipmentApi() {
    this.init();
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.apiWebClient = new ApiWebClient(authenticationsApi);
  }

  /**
   * POST /shipment/{shipment-id}: ""
   * @param idempotencyKey the idempotency key true@param sortBy the field to sort by false@param sessionId the session cookie false@param shipmentId the shipment to update true
   * @param shipment (required)
   * @return ok; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  private ResponseSpec createShipmentRequestCreation(String idempotencyKey, String sortBy, String sessionId, String shipmentId, Shipment shipment) throws WebClientResponseException {
    Object postBody = shipment;
    if (shipment == null) {
    throw new WebClientResponseException("Missing the required parameter ''shipment'' when calling createShipment", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
  }
    final Map<String, Object> pathParams = new HashMap<String, Object>();

    pathParams.put("shipment-id",  shipmentId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiWebClient.parameterToMultiValueMap( null, "sort-by", sortBy));

    if (idempotencyKey != null) {
      headerParams.add("Idempotency-Key", apiWebClient.parameterToString(idempotencyKey));
    }

    cookieParams.putAll(apiWebClient.parameterToMultiValueMap( null, "session.id", sessionId));
    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiWebClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiWebClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Shipment> localVarReturnType = new ParameterizedTypeReference<Shipment>() {};
    return apiWebClient.invokeAPI("","/shipment/{shipment-id}", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);

  }

  /**
   * POST /shipment/{shipment-id}
   * @param idempotencyKey the idempotency key (required)
   * @param sortBy the field to sort by 
   * @param sessionId the session cookie 
   * @param shipmentId the shipment to update (required)
   * @param shipment   (required)
   * @return ok; (status code 200)
   * @throws WebClientResponseException if an error occurs while attempting to invoke the API
   */
  public Mono<Shipment> createShipment(String idempotencyKey, String sortBy, String sessionId, String shipmentId, Shipment shipment) throws WebClientResponseException {
    ParameterizedTypeReference<Shipment> localVarReturnType = new ParameterizedTypeReference<Shipment>() {};
    return createShipmentRequestCreation(idempotencyKey, sortBy, sessionId, shipmentId, shipment).bodyToMono(localVarReturnType);
  }

  public Mono<ResponseEntity<Shipment>> createShipmentWithHttpInfo(String idempotencyKey, String sortBy, String sessionId, String shipmentId, Shipment shipment) throws WebClientResponseException {
    ParameterizedTypeReference<Shipment> localVarReturnType = new ParameterizedTypeReference<Shipment>() {};
    return createShipmentRequestCreation(idempotencyKey, sortBy, sessionId, shipmentId, shipment).toEntity(localVarReturnType);
  }

}