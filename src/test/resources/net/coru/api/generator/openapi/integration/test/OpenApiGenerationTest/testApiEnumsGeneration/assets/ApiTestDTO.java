package net.coru.multifileplugin.testapi.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;

public class ApiTestDTO {

  @JsonProperty(value ="name")
  private String name;
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
    BIG_DECIMAL_3_DOT_3(new BigDecimal("3.3")),
    BIG_DECIMAL_1_DOT_1(new BigDecimal("1.1")),
    BIG_DECIMAL_2_DOT_2(new BigDecimal("2.2"));

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
  @JsonProperty(value ="id")
  private Integer id;
  @JsonProperty(value ="unionEnum")
  private UnionEnum unionEnum;

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

  private ApiTestDTO(ApiTestDTOBuilder builder) {
    this.name = builder.name;
    this.unionIntegerEnum = builder.unionIntegerEnum;
    this.unionNumberEnum = builder.unionNumberEnum;
    this.id = builder.id;
    this.unionEnum = builder.unionEnum;
  }

  public static class ApiTestDTOBuilder {

    private String name;
    private UnionIntegerEnum unionIntegerEnum;
    private UnionNumberEnum unionNumberEnum;
    private Integer id;
    private UnionEnum unionEnum;

    public ApiTestDTO.ApiTestDTOBuilder name(String name) {
      this.name = name;
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

    public ApiTestDTO.ApiTestDTOBuilder id(Integer id) {
      this.id = id;
      return this;
    }
    public ApiTestDTO.ApiTestDTOBuilder unionEnum(UnionEnum unionEnum) {
      this.unionEnum = unionEnum;
      return this;
    }

    public ApiTestDTO build() {
      ApiTestDTO apiTestDTO =  new ApiTestDTO(this);
      return apiTestDTO;
    }
  }

  /**
  * Get name
  * @return name
  */
  @ApiModelProperty( value = "description")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }

  /**
  * Get unionIntegerEnum
  * @return unionIntegerEnum
  */
  @ApiModelProperty( value = "description")
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
  @ApiModelProperty( value = "description")
  public UnionNumberEnum getUnionNumberEnum() {
    return unionNumberEnum;
  }
  public void setUnionNumberEnum(UnionNumberEnum unionNumberEnum) {
    this.unionNumberEnum = unionNumberEnum;
  }

  /**
  * Get id
  * @return id
  */
  @ApiModelProperty( value = "description")
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
    this.id = id;
  }

  /**
  * Get unionEnum
  * @return unionEnum
  */
  @ApiModelProperty( value = "description")
  public UnionEnum getUnionEnum() {
    return unionEnum;
  }
  public void setUnionEnum(UnionEnum unionEnum) {
    this.unionEnum = unionEnum;
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
    return Objects.equals(this.name,apiTestDTO.name) && Objects.equals(this.unionIntegerEnum,apiTestDTO.unionIntegerEnum) && Objects.equals(this.unionNumberEnum,apiTestDTO.unionNumberEnum) && Objects.equals(this.id,apiTestDTO.id) && Objects.equals(this.unionEnum,apiTestDTO.unionEnum) ;
  }

  @Override
  public int hashCode() {
    return Objects.hash(name,unionIntegerEnum,unionNumberEnum,id,unionEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTestDTO {\n");
    sb.append(" name: ").append(toIndentedString(name)).append("\n");
    sb.append(" unionIntegerEnum: ").append(toIndentedString(unionIntegerEnum)).append("\n");
    sb.append(" unionNumberEnum: ").append(toIndentedString(unionNumberEnum)).append("\n");
    sb.append(" id: ").append(toIndentedString(id)).append("\n");
    sb.append(" unionEnum: ").append(toIndentedString(unionEnum)).append("\n");
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

}
