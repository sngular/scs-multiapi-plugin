package com.sngular.multifileplugin.testreferencefilenocomponents;

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

import com.sngular.multifileplugin.testreferencefilenocomponents.model.InlineResponse200TestFileDTO;

public interface TestApi {

  /**
   * GET /test
   * @return  Some schema from another file; (status code 200)
   */

  @Operation(
    operationId = "testFile",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Some schema from another file", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InlineResponse200TestFileDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test",
    produces = {"application/json"}
  )

  default ResponseEntity<InlineResponse200TestFileDTO> testFile() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
