package com.sngular.multifileplugin.testrarecharsnamesrestclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sngular.multifileplugin.testrarecharsnamesrestclient.client.ApiRestClient;

import com.sngular.multifileplugin.testrarecharsnamesrestclient.model.Shipment;

import com.sngular.multifileplugin.testrarecharsnamesrestclient.client.auth.Authentication;

import org.springframework.stereotype.Component;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;

@Component()
public class ShipmentApi {

  private ApiRestClient apiRestClient;

  private Map<String, Authentication> authenticationsApi;

  public ShipmentApi() {
    this.init();
  }

  protected void init() {
    this.authenticationsApi = new HashMap<String, Authentication>();
    this.apiRestClient = new ApiRestClient(authenticationsApi);
  }

  /**
   * POST /shipment/{shipment-id}
   * @param idempotencyKey the idempotency key (required)
   * @param sortBy the field to sort by 
   * @param sessionId the session cookie 
   * @param shipmentId the shipment to update (required)
   * @param shipment  (required)
   * @return ok; (status code 200)
   * @throws RestClientException if an error occurs while attempting to invoke the API
   */
  public Shipment createShipment(String idempotencyKey, String sortBy, String sessionId, String shipmentId, Shipment shipment ) throws RestClientException {
    return createShipmentWithHttpInfo(idempotencyKey, sortBy, sessionId, shipmentId, shipment).getBody();
  }

  public ResponseEntity<Shipment> createShipmentWithHttpInfo(String idempotencyKey, String sortBy, String sessionId, String shipmentId, Shipment shipment) throws RestClientException {

    Object postBody = shipment;
    if (shipment == null) {
      throw new RestClientException(HttpStatus.BAD_REQUEST + " Missing the required parameter ''shipment'' when calling createShipment");
    }
    final Map<String, Object> uriVariables = new HashMap<String, Object>();

    uriVariables.put("shipment-id",  shipmentId);
    final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
    final HttpHeaders headerParams = new HttpHeaders();
    final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
    final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

    queryParams.putAll(apiRestClient.parameterToMultiValueMap( null, "sort-by", sortBy));
    if (idempotencyKey != null) {
      headerParams.add("Idempotency-Key", apiRestClient.parameterToString(idempotencyKey));
    }

    cookieParams.putAll(apiRestClient.parameterToMultiValueMap( null, "session.id", sessionId));

    final String[] localVarAccepts = {"application/json"};
    final List<MediaType> localVarAccept = apiRestClient.selectHeaderAccept(localVarAccepts);
    final String[] localVarContentTypes = {"application/json"};
    final MediaType localVarContentType = apiRestClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {};

    ParameterizedTypeReference<Shipment> localVarReturnType = new ParameterizedTypeReference<Shipment>() {};
    return apiRestClient.invokeAPI("","/shipment/{shipment-id}", HttpMethod.POST, uriVariables, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
  }

}