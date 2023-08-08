package com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject;

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

import com.sngular.multifileplugin.testadditionalpropertiesWithUnnamedObject.model.TestDTO;

public interface TestApi {

  /**
   * GET /test: testAdditionalProperties
   * @return  An object with additional properties; (status code 200)
   */

  @Operation(
    operationId = "testAdditionalProperties",
    summary = "testAdditionalProperties",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "An object with additional properties", content = @Content(mediaType = "application/json", schema = @Schema(implementation = TestDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test",
    produces = {"application/json"}
  )

  default ResponseEntity<TestDTO> testAdditionalProperties() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
