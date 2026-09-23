package com.sngular.multifileplugin.testreftoallofproperty;

import java.util.Optional;
import java.util.List;
import java.util.Map;
import javax.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.MediaType;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.NativeWebRequest;

import com.sngular.multifileplugin.testreftoallofproperty.model.ClientSearchResultDTO;

public interface ClientsApi {

  /**
   * GET /clients/search
   * @return  The client lookup result; (status code 200)
   */

  @Operation(
    operationId = "searchClient",
    tags = {},
    responses = {
      @ApiResponse(responseCode = "200", description = "The client lookup result", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ClientSearchResultDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/clients/search",
    produces = {"application/json"}
  )

  default ResponseEntity<ClientSearchResultDTO> searchClient() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
