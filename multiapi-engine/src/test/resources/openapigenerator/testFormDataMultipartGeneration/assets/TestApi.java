package com.sngular.multifileplugin.testformdatamultipartgeneration;

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

import com.sngular.multifileplugin.testFormDataMultipartGeneration.InlineObjectTestMultipart;

public interface TestApi {

  /**
   * GET /test
   * @param inlineObjectTestMultipart (required)
   * @return  OK; (status code 200)
   */

  @Operation(
    operationId = "testMultipart",
    tags = {"test"},
    responses = {
      @ApiResponse(responseCode = "200", description = "OK")
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> testMultipart(@Parameter(name = "inlineObjectTestMultipart", description = "", required = true, schema = @Schema(description = "")) @Valid InlineObjectTestMultipart inlineObjectTestMultipart) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
