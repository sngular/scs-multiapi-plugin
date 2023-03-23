package com.sngular.multifileplugin.testDateTimeZoned;

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

import com.sngular.multifileplugin.testDateTimeZoned.model.TestDateDTO;

public interface TestApi {

  /**
  * GET /test 
  * @return  An object with date and date-time formats; (status code 200)
  */

  @Operation(
     operationId = "testDateTime",
     tags = {"test"},
     responses = {
       @ApiResponse(responseCode = "200", description = "An object with date and date-time formats", content = @Content(mediaType = "application/json", schema = @Schema(implementation = TestDateDTO.class)))
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test",
    produces = {"application/json"}
  )

  default ResponseEntity<TestDateDTO> testDateTime() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
