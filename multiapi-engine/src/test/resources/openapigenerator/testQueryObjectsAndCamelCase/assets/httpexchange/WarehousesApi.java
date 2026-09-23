package com.sngular.multifileplugin.queryobjectshttpexchange;

import java.util.List;
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

import com.sngular.multifileplugin.queryobjectshttpexchange.model.WarehousePageDTO;
import com.sngular.multifileplugin.queryobjectshttpexchange.model.PageFilterDTO;

/**
 * Warehouses API, as a Spring HTTP service interface. Back it with a configured {@code RestClient}, whose
 * base URL, authentication, timeouts and message converters apply to every request:
 * <pre>
 * HttpServiceProxyFactory.builderFor(RestClientAdapter.create(restClient)).build()
 *     .createClient(WarehousesApi.class);
 * </pre>
 * or, on Spring Boot 4, register it with {@code @ImportHttpServices}.
 */
@HttpExchange
public interface WarehousesApi {

  /**
   * GET /warehouses
   * @param page_number 
   * @param page_size 
   * @param sort_by 
   * @param warehouse_ids 
   * @return The warehouses (status code 200);
   */
  @GetExchange(url = "/warehouses", accept = {"application/json"})
  ResponseEntity<WarehousePageDTO> searchWarehouses(@RequestParam(name = "page_number", required = false) Integer page_number, @RequestParam(name = "page_size", required = false) Integer page_size, @RequestParam(name = "sort_by", required = false) String sort_by, @RequestParam(name = "warehouse_ids", required = false) List<Long> warehouse_ids);

  /**
   * GET /warehouses/by-flat-filter
   * @param filters 
   * @return The warehouses (status code 200);
   */
  @GetExchange(url = "/warehouses/by-flat-filter", accept = {"application/json"})
  ResponseEntity<WarehousePageDTO> searchWarehousesFlat(@RequestParam(name = "filters", required = false) PageFilterDTO filters);

  /**
   * GET /warehouses/by-deep-filter
   * @param filtersPageNumber 
   * @param filtersPageSize 
   * @return The warehouses (status code 200);
   */
  @GetExchange(url = "/warehouses/by-deep-filter", accept = {"application/json"})
  ResponseEntity<WarehousePageDTO> searchWarehousesDeep(@RequestParam(name = "filters[page_number]", required = false) Integer filtersPageNumber, @RequestParam(name = "filters[page_size]", required = false) Integer filtersPageSize);
}
