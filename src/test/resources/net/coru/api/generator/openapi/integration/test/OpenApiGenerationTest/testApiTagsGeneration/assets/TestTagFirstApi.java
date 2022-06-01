package net.coru.multifileplugin.testtags;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.coru.multifileplugin.testtags.model.TestDTO;
import net.coru.multifileplugin.testtags.model.ErrorDTO;


public interface TestTagFirstApi {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

         /**
         * GET /test : List all available testServer
         *
         *
         * @return  A paged array of tests; (status code 200)
         */

         @Operation(
            operationId = "listTestServer",
            summary = "List all available testServer",
            tags = { "TestTagFirst" },
            responses = {
                 @ApiResponse(responseCode = "200", description = "A paged array of tests" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = TestDTO.class)) ), @ApiResponse(responseCode = "default", description = "unexpected error" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = ErrorDTO.class)) )
            }
         )
         @RequestMapping(
           method = RequestMethod.GET,
           value = "/test",
           produces = { "application/json" }
         )


         default ResponseEntity<TestDTO> listTestServer () {
                return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
          }



}
