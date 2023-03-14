package com.sngular.multifileplugin.enumgeneration.model;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import com.sngular.multifileplugin.enumgeneration.model.exception.ModelClassException;
import com.sngular.multifileplugin.enumgeneration.model.customvalidator.NotNull;

public class ApiTestDTO {

  @JsonProperty(value ="unionEnum")
  @NotNull
  private final UnionEnum unionEnum;
  public enum UnionEnum {
    ONEOF("oneof"),
    ANYOF("anyof"),
    ALLOF("allof");

    private String value;

    UnionEnum(String value) {
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
  @JsonProperty(value ="name")
  @NotNull
  private final String name;
  @JsonProperty(value ="id")
  @NotNull
  private final Integer id;
  @JsonProperty(value ="unionIntegerEnum")
  private UnionIntegerEnum unionIntegerEnum;
  public enum UnionIntegerEnum {
    LONG_1(1l),
    LONG_2(2l),
    LONG_3(3l);

    private Long value;

    UnionIntegerEnum(Long value) {
      this.value = value;
    }

    @JsonValue
    public Long getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  @JsonProperty(value ="unionNumberEnum")
  private UnionNumberEnum unionNumberEnum;
  public enum UnionNumberEnum {
    BIG_DECIMAL_1_DOT_1(new BigDecimal("1.1")),
    BIG_DECIMAL_2_DOT_2(new BigDecimal("2.2")),
    BIG_DECIMAL_4_DOT_4(new BigDecimal("4.4"));

    private BigDecimal value;

    UnionNumberEnum(BigDecimal value) {
      this.value = value;
    }

    @JsonValue
    public BigDecimal getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  private ApiTestDTO(UnionEnum unionEnum, String name, Integer id, UnionIntegerEnum unionIntegerEnum, UnionNumberEnum unionNumberEnum) {
    this.unionEnum = unionEnum;
    this.name = name;
    this.id = id;
    this.unionIntegerEnum = unionIntegerEnum;
    this.unionNumberEnum = unionNumberEnum;

    validateRequiredAttributes();
  }

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.unionEnum = builder.unionEnum;
    this.name = builder.name;
    this.id = builder.id;
    this.unionIntegerEnum = builder.unionIntegerEnum;
    this.unionNumberEnum = builder.unionNumberEnum;

    validateRequiredAttributes();
  }

  public static ApiTestDTO.ApiTestDTOBuilder builder() {
    return new ApiTestDTO.ApiTestDTOBuilder();
  }

  public static class ApiTestDTOBuilder {

    private UnionEnum unionEnum;
    private String name;
    private Integer id;
    private UnionIntegerEnum unionIntegerEnum;
    private UnionNumberEnum unionNumberEnum;
    public ApiTestDTO.ApiTestDTOBuilder unionEnum(UnionEnum unionEnum) {
      this.unionEnum = unionEnum;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder name(String name) {
      this.name = name;
      return this;
    }

    public ApiTestDTO.ApiTestDTOBuilder id(Integer id) {
      this.id = id;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder unionIntegerEnum(UnionIntegerEnum unionIntegerEnum) {
      this.unionIntegerEnum = unionIntegerEnum;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder unionNumberEnum(UnionNumberEnum unionNumberEnum) {
      this.unionNumberEnum = unionNumberEnum;
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO = new ApiTestDTO(this);
      return apiTestDTO;
    }
  }

  /**
  * Get unionEnum
  * @return unionEnum
  */
  @Schema(name = "unionEnum", required = true)
  public UnionEnum getUnionEnum() {
    return unionEnum;
  }

  /**
  * Get name
  * @return name
  */
  @Schema(name = "name", required = true)
  public String getName() {
    return name;
  }

  /**
  * Get id
  * @return id
  */
  @Schema(name = "id", required = true)
  public Integer getId() {
    return id;
  }

  /**
  * Get unionIntegerEnum
  * @return unionIntegerEnum
  */
  @Schema(name = "unionIntegerEnum", required = false)
  public UnionIntegerEnum getUnionIntegerEnum() {
    return unionIntegerEnum;
  }
  public void setUnionIntegerEnum(UnionIntegerEnum unionIntegerEnum) {
    this.unionIntegerEnum = unionIntegerEnum;
  }

  /**
  * Get unionNumberEnum
  * @return unionNumberEnum
  */
  @Schema(name = "unionNumberEnum", required = false)
  public UnionNumberEnum getUnionNumberEnum() {
    return unionNumberEnum;
  }
  public void setUnionNumberEnum(UnionNumberEnum unionNumberEnum) {
    this.unionNumberEnum = unionNumberEnum;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ApiTestDTO apiTestDTO = (ApiTestDTO) o;
    return Objects.equals(this.unionEnum, apiTestDTO.unionEnum) && Objects.equals(this.name, apiTestDTO.name) && Objects.equals(this.id, apiTestDTO.id) && Objects.equals(this.unionIntegerEnum, apiTestDTO.unionIntegerEnum) && Objects.equals(this.unionNumberEnum, apiTestDTO.unionNumberEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(unionEnum, name, id, unionIntegerEnum, unionNumberEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestDTO {\n");
    sb.append(" unionEnum: ").append(toIndentedString(unionEnum)).append("\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" unionIntegerEnum: ").append(toIndentedString(unionIntegerEnum)).append("\n");
    sb.append(" unionNumberEnum: ").append(toIndentedString(unionNumberEnum)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }


  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

    if (!Objects.nonNull(this.unionEnum)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.name)) {
      satisfiedCondition = false;
    } else if (!Objects.nonNull(this.id)) {
      satisfiedCondition = false;
    }

    if (!satisfiedCondition) {
      throw new ModelClassException("ApiTestDTO");
    }
  }

}
