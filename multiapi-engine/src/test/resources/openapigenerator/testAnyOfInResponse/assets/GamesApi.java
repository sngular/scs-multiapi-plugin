package net.coru.multifileplugin.testanyofinresponse;
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

import net.coru.multifileplugin.testanyofinresponse.model.InlineResponse200ListGamesAnyOfDTO;

public interface GamesApi {

  /**
  * GET /games : List all available games
  * @return  A paged array of games; (status code 200)
  */

  @Operation(
     operationId = "listGames",
     summary = "List all available games",
     tags = { "games" },
     responses = {
          @ApiResponse(responseCode = "200", description = "A paged array of games" , content = @Content(mediaType = "application/json" ,schema = @Schema(implementation = List.class)) )
     }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/games",
    produces = { "application/json" }
  )

  default ResponseEntity<InlineResponse200ListGamesAnyOfDTO> listGames() {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
