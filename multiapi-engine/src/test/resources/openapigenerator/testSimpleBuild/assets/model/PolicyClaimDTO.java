package com.sngular.multifileplugin.testsimplebuild.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.extern.jackson.Jacksonized;

@Data
public class PolicyClaimDTO {

  @JsonProperty(value ="proofOfDeliveryDocument")
  private DocumentDTO proofOfDeliveryDocument;

  @JsonProperty(value ="invoiceDocument")
  private DocumentDTO invoiceDocument;

  @JsonProperty(value ="claimKind")
  private String claimKind;

  @JsonProperty(value ="merchantContactDetails")
  private ContactDTO merchantContactDetails;

  @JsonProperty(value ="claimDocument")
  private DocumentDTO claimDocument;

  @JsonProperty(value ="debtorContactDetails")
  private ContactDTO debtorContactDetails;


  @Builder
  @Jacksonized
  private PolicyClaimDTO(DocumentDTO proofOfDeliveryDocument, DocumentDTO invoiceDocument, String claimKind, ContactDTO merchantContactDetails, DocumentDTO claimDocument, ContactDTO debtorContactDetails) {
    this.proofOfDeliveryDocument = proofOfDeliveryDocument;
    this.invoiceDocument = invoiceDocument;
    this.claimKind = claimKind;
    this.merchantContactDetails = merchantContactDetails;
    this.claimDocument = claimDocument;
    this.debtorContactDetails = debtorContactDetails;

  }

}
