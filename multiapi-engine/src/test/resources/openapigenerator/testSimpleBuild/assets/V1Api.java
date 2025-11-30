package com.sngular.multifileplugin.testsimplebuild;

import java.lang.String;
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

import com.sngular.multifileplugin.testsimplebuild.model.CompanyDTO;
import com.sngular.multifileplugin.testsimplebuild.model.ErrorResponseDTO;
import com.sngular.multifileplugin.testsimplebuild.model.CreditLimitDTO;
import com.sngular.multifileplugin.testsimplebuild.model.QuoteRequestDTO;
import com.sngular.multifileplugin.testsimplebuild.model.QuoteResponseDTO;
import com.sngular.multifileplugin.testsimplebuild.model.UpdateQuoteDTO;
import com.sngular.multifileplugin.testsimplebuild.model.QuoteUpdateResponseDTO;
import com.sngular.multifileplugin.testsimplebuild.model.ActivatePolicyDTO;
import com.sngular.multifileplugin.testsimplebuild.model.PolicyActivationDTO;
import com.sngular.multifileplugin.testsimplebuild.model.PolicySettlementDTO;
import com.sngular.multifileplugin.testsimplebuild.model.PolicyClaimDTO;

public interface V1Api {

  /**
   * GET /v1/company/{companyId}/info: Retrieve info from a company
   * @param countryCode Country code according to ISO 3166 true @param vatCode The unique VAT code that identifies the company true @param companyId The company database identifier true
   * @return  Successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "getCompanyInfo",
    summary = "Retrieve info from a company",
    tags = {"Company"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.GET,
    value = "/v1/company/{companyId}/info",
    produces = {"application/json"}
  )

  default ResponseEntity<List<CompanyDTO>> getCompanyInfo(@Parameter(name = "countryCode", description = "Country code according to ISO 3166", required = true, schema = @Schema(description = "")) @RequestParam(required = true) String countryCode , @Parameter(name = "vatCode", description = "The unique VAT code that identifies the company", required = true, schema = @Schema(description = "")) @RequestParam(required = true) String vatCode , @Parameter(name = "companyId", description = "The company database identifier", required = true, schema = @Schema(description = "")) @PathVariable("companyId") Integer companyId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /v1/company/{companyId}/creditLimit: Compute the credit limits for a company
   * @param companyId Company database identifier true
   * @return  Successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "getCompanyCreditLimit",
    summary = "Compute the credit limits for a company",
    tags = {"Company"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/company/{companyId}/creditLimit",
    produces = {"application/json"}
  )

  default ResponseEntity<List<CreditLimitDTO>> getCompanyCreditLimit(@Parameter(name = "companyId", description = "Company database identifier", required = true, schema = @Schema(description = "")) @PathVariable("companyId") Integer companyId) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /v1/insurance: Request a quote to insure an invoice
   * @param quoteRequestDTO
   * @return  successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "obtainInsuranceQuote",
    summary = "Request a quote to insure an invoice",
    tags = {"Insurance"},
    responses = {
      @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/insurance",
    produces = {"application/json"}
  )

  default ResponseEntity<List<QuoteResponseDTO>> obtainInsuranceQuote(@Parameter(name = "quoteRequestDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody QuoteRequestDTO quoteRequestDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * PUT /v1/insurance/update/{quoteId}: Update the invoice number and invoice date for a certain quote
   * @param quoteId true
   * @param updateQuoteDTO
   * @return  Successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "updateInvoiceNumberAndDateForInsurance",
    summary = "Update the invoice number and invoice date for a certain quote",
    tags = {"Insurance"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = QuoteUpdateResponseDTO.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.PUT,
    value = "/v1/insurance/update/{quoteId}",
    produces = {"application/json"}
  )

  default ResponseEntity<QuoteUpdateResponseDTO> updateInvoiceNumberAndDateForInsurance(@Parameter(name = "quoteId", required = true, schema = @Schema(description = "")) @PathVariable("quoteId") Integer quoteId , @Parameter(name = "updateQuoteDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody UpdateQuoteDTO updateQuoteDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /v1/insurance/activate/{quoteId}: Activate a quote to insure an invoice
   * @param quoteId true
   * @param activatePolicyDTO
   * @return  Successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "activate",
    summary = "Activate a quote to insure an invoice",
    tags = {"Insurance"},
    responses = {
      @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = PolicyActivationDTO.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/insurance/activate/{quoteId}",
    produces = {"application/json"}
  )

  default ResponseEntity<PolicyActivationDTO> activate(@Parameter(name = "quoteId", required = true, schema = @Schema(description = "")) @PathVariable("quoteId") Integer quoteId , @Parameter(name = "activatePolicyDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody ActivatePolicyDTO activatePolicyDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /v1/insurance/settle/{policyId}: Settle a policy that was previously activated
   * @param policyIdInProvider ID of the policy for the specific insurance provider true
   * @param policySettlementDTO
   * @return  successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "settle",
    summary = "Settle a policy that was previously activated",
    tags = {"Insurance"},
    responses = {
      @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/insurance/settle/{policyId}",
    produces = {"application/json"}
  )

  default ResponseEntity<List<PolicySettlementDTO>> settle(@Parameter(name = "policyIdInProvider", description = "ID of the policy for the specific insurance provider", required = true, schema = @Schema(description = "")) @PathVariable("policyIdInProvider") String policyIdInProvider , @Parameter(name = "policySettlementDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody PolicySettlementDTO policySettlementDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }
  /**
   * POST /v1/insurance/claim/{policyId}: Claim a policy that was unpaid
   * @param operationId ID of the operation in our own records, for reconciliation purposes true @param provider Name of the insurance provider false @param policyId ID of the policy for the specific insurance provider true @param claimDate Date that we start the claim true
   * @param policyClaimDTO
   * @return  successful operation; (status code 200)  Bad request; (status code 400)  Not found; (status code 404)
   */

  @Operation(
    operationId = "claim",
    summary = "Claim a policy that was unpaid",
    tags = {"Insurance"},
    responses = {
      @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(mediaType = "application/json", schema = @Schema(implementation = List.class))),
      @ApiResponse(responseCode = "400", description = "Bad request", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class))),
      @ApiResponse(responseCode = "404", description = "Not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = ErrorResponseDTO.class)))
    }
  )
  @RequestMapping(
    method = RequestMethod.POST,
    value = "/v1/insurance/claim/{policyId}",
    produces = {"application/json"}
  )

  default ResponseEntity<List<PolicySettlementDTO>> claim(@Parameter(name = "operationId", description = "ID of the operation in our own records, for reconciliation purposes", required = true, schema = @Schema(description = "")) @RequestParam(required = true) Integer operationId , @Parameter(name = "provider", description = "Name of the insurance provider", required = false, schema = @Schema(description = "")) @RequestParam(required = false) String provider , @Parameter(name = "policyId", description = "ID of the policy for the specific insurance provider", required = true, schema = @Schema(description = "")) @PathVariable("policyId") String policyId , @Parameter(name = "claimDate", description = "Date that we start the claim", required = true, schema = @Schema(description = "")) @RequestParam(required = true) String claimDate , @Parameter(name = "policyClaimDTO", description = "", required = false, schema = @Schema(description = "")) @Valid @RequestBody PolicyClaimDTO policyClaimDTO) {
    return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
  }

}
