package com.sngular.multifileplugin.webhooks;

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

import com.sngular.multifileplugin.webhooks.model.PetDTO;

public interface NewPetApi {

  /**
   * POST /newPet: New pet notification
   * @param petDTO
   * @return  Notification acknowledged; (status code 200)
   */

  @Operation(
    operationId = "newPetWebhook",
    summary = "New pet notification",
    tags = {"newPet"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Notification acknowledged")
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/newPet",
    produces = {"application/json"}
  )

  default ResponseEntity<Void> newPetWebhook(@Parameter(name = "petDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody(required = false) PetDTO petDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
