package net.coru.multifileplugin.tagsgeneration;
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
import javax.validation.Valid;
import java.util.Optional;
import java.util.List;
import java.util.Map;

import net.coru.multifileplugin.tagsgeneration.model.TestInfoDTO;
import net.coru.multifileplugin.tagsgeneration.model.ErrorDTO;

public interface TestTagSecondApi {

  default Optional<NativeWebRequest> getRequest() {
    return Optional.empty();
  }

  /**
  * GET /test/{testId} : Info for a specific test
  * @param testId The id of the test to retrieve true
  * @return  Expected response to a valid request; (status code 200)
  */

  @Operation(
     operationId = "showTestById",
     summary = "Info for a specific test",
     tags = { "TestTagSecond" },
     responses = {
          @ApiResponse(responseCode = "200", description = "Expected response to a valid request" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = TestInfoDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ErrorDTO.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/test/{testId}",
    produces = { "application/json" }
  )

  default ResponseEntity<TestInfoDTO> showTestById(@Parameter(name = "testId", description = "The id of the test to retrieve", required = true, schema = @Schema(description = "")) @PathVariable("testId") Integer testId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
