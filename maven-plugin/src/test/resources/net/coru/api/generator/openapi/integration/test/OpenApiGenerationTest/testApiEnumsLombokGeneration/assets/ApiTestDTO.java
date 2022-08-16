package net.coru.api.generator.openapi.integration.test.OpenApiGenerationTest.testApiEnumsLombokGeneration.assets;

import java.math.BigDecimal;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;


@Data
public class ApiTestDTO {

  @JsonProperty(value ="name")
  @NonNull
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
  @NonNull
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


  @Builder
  private ApiTestDTO(@NonNull String name, UnionIntegerEnum unionIntegerEnum, UnionNumberEnum unionNumberEnum, @NonNull Integer id, UnionEnum unionEnum){
    this.name = name;
    this.unionIntegerEnum = unionIntegerEnum;
    this.unionNumberEnum = unionNumberEnum;
    this.id = id;
    this.unionEnum = unionEnum;

  }

}
