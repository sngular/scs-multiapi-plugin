package com.sngular.multifileplugin.openapi31completeness.model;

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import org.springframework.web.multipart.MultipartFile;
import java.util.List;
import java.util.ArrayList;
import java.math.BigDecimal;
import java.util.Map;
import java.util.HashMap;

@JsonDeserialize(builder = GadgetDTO.GadgetDTOBuilder.class)
public class GadgetDTO {

  @JsonProperty(value ="status")
  private Status status;
  public enum Status {
    ACTIVE("ACTIVE"),
    RETIRED("RETIRED");

    private String value;

    Status(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="legacyId")
  private String legacyId;
  @JsonProperty(value ="payload")
  private MultipartFile payload;
  @JsonProperty(value ="nothing")
  private Object nothing;
  @JsonProperty(value ="id")
  private String id;
  @JsonProperty(value ="metadata")
  private Map<String, String> metadata;
  @JsonProperty(value ="coords")
  private List<BigDecimal> coords;
  @JsonProperty(value ="owner")
  private PersonDTO owner;
  @JsonProperty(value ="serial")
  private String serial;

  private GadgetDTO(GadgetDTOBuilder builder) {
    this.status = builder.status;
    this.legacyId = builder.legacyId;
    this.payload = builder.payload;
    this.nothing = builder.nothing;
    this.id = builder.id;
    this.metadata = builder.metadata;
    this.coords = builder.coords;
    this.owner = builder.owner;
    this.serial = builder.serial;

  }

  public static GadgetDTO.GadgetDTOBuilder builder() {
    return new GadgetDTO.GadgetDTOBuilder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class GadgetDTOBuilder {

    private Status status;
    private String legacyId;
    private MultipartFile payload;
    private Object nothing;
    private String id;
    private Map<String, String> metadata = new HashMap<String, String>();
    private List<BigDecimal> coords = new ArrayList<BigDecimal>();
    private PersonDTO owner;
    private String serial;

    public GadgetDTO.GadgetDTOBuilder status(Status status) {
      this.status = status;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder legacyId(String legacyId) {
      this.legacyId = legacyId;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder payload(MultipartFile payload) {
      this.payload = payload;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder nothing(Object nothing) {
      this.nothing = nothing;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder id(String id) {
      this.id = id;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder metadata(Map<String, String> metadata) {
      this.metadata = metadata;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder metadataValue(String key, String value) {
      this.metadata.put(key, value);
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder coords(List<BigDecimal> coords) {
      if (Objects.nonNull(coords) && !coords.isEmpty()) {
        this.coords.addAll(coords);
      }
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder coord(BigDecimal coord) {
      if (Objects.nonNull(coord)) {
        this.coords.add(coord);
      }
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder owner(PersonDTO owner) {
      this.owner = owner;
      return this;
    }

    public GadgetDTO.GadgetDTOBuilder serial(String serial) {
      this.serial = serial;
      return this;
    }

    public GadgetDTO build() {
      GadgetDTO gadgetDTO = new GadgetDTO(this);
      return gadgetDTO;
    }
  }

  @Schema(name = "status", required = false, description = "Lifecycle status of the gadget")
  public Status getStatus() {
    return status;
  }
  public void setStatus(Status status) {
    this.status = status;
  }

  @Schema(name = "legacyId", required = false, deprecated = true)
  public String getLegacyId() {
    return legacyId;
  }
  public void setLegacyId(String legacyId) {
    this.legacyId = legacyId;
  }

  @Schema(name = "payload", required = false)
  public MultipartFile getPayload() {
    return payload;
  }
  public void setPayload(MultipartFile payload) {
    this.payload = payload;
  }

  @Schema(name = "nothing", required = false)
  public Object getNothing() {
    return nothing;
  }
  public void setNothing(Object nothing) {
    this.nothing = nothing;
  }

  @Schema(name = "id", required = false, description = "The unique gadget identifier", example = "gadget-001")
  public String getId() {
    return id;
  }
  public void setId(String id) {
    this.id = id;
  }

  @Schema(name = "metadata", required = false)
  public Map<String, String> getMetadata() {
    return metadata;
  }
  public void setMetadata(Map<String, String> metadata) {
    this.metadata = metadata;
  }

  @Schema(name = "coords", required = false)
  public List<BigDecimal> getCoords() {
    return coords;
  }
  public void setCoords(List<BigDecimal> coords) {
    this.coords = coords;
  }

  @Schema(name = "owner", required = false)
  public PersonDTO getOwner() {
    return owner;
  }
  public void setOwner(PersonDTO owner) {
    this.owner = owner;
  }

  @Schema(name = "serial", required = false, example = "SN-12345")
  public String getSerial() {
    return serial;
  }
  public void setSerial(String serial) {
    this.serial = serial;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    GadgetDTO gadgetDTO = (GadgetDTO) o;
    return Objects.equals(this.status, gadgetDTO.status) && Objects.equals(this.legacyId, gadgetDTO.legacyId) && Objects.equals(this.payload, gadgetDTO.payload) && Objects.equals(this.nothing, gadgetDTO.nothing) && Objects.equals(this.id, gadgetDTO.id) && Objects.equals(this.metadata, gadgetDTO.metadata) && Objects.equals(this.coords, gadgetDTO.coords) && Objects.equals(this.owner, gadgetDTO.owner) && Objects.equals(this.serial, gadgetDTO.serial);
  }

  @Override
  public int hashCode() {
    return Objects.hash(status, legacyId, payload, nothing, id, metadata, coords, owner, serial);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("GadgetDTO{");
    sb.append(" status:").append(status).append(",");
    sb.append(" legacyId:").append(legacyId).append(",");
    sb.append(" payload:").append(payload).append(",");
    sb.append(" nothing:").append(nothing).append(",");
    sb.append(" id:").append(id).append(",");
    sb.append(" metadata:").append(metadata).append(",");
    sb.append(" coords:").append(coords).append(",");
    sb.append(" owner:").append(owner).append(",");
    sb.append(" serial:").append(serial);
    sb.append("}");
    return sb.toString();
  }


}
