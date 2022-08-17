package net.coru.api.generator.plugin.openapi;

import static net.coru.api.generator.test.utils.TestUtils.validateFiles;
import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import net.coru.api.generator.plugin.openapi.parameter.FileSpec;
import org.apache.commons.collections4.CollectionUtils;

public final class OpenApiGeneratorFixtures {

  final static String GENERATED = "generated";

  final static String TARGET = "target";

  final static List<FileSpec> TEST_ALL_OF = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testAllOf/api-test.yml")
      .apiPackage("net.coru.multifileplugin.testapi")
      .modelPackage("net.coru.multifileplugin.testapi.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .build()
  );

  final static List<FileSpec> TEST_ALL_OF_LOMBOK = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testAllOf/api-test.yml")
      .apiPackage("net.coru.multifileplugin.lombok.testapi")
      .modelPackage("net.coru.multifileplugin.lombok.testapi.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(true)
      .build()
  );

  final static List<FileSpec> TEST_OVER_WRITE_MODEL_TRUE =  List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testOverWriteModelTrue/api-test.yml")
      .apiPackage("net.coru.multifileplugin.testoverwriteapi")
      .modelPackage("net.coru.multifileplugin.testoverwriteapi.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(false)
      .build()
  );

  static final List<FileSpec> TEST_API_CLIENT_GENERATION = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testApiClientGeneration/api-test.yml")
      .apiPackage("net.coru.multifileplugin.testapiclient")
      .modelPackage("net.coru.multifileplugin.testapiclient.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(true)
      .build()
  );

  static final List<FileSpec> TEST_INLINE_SCHEMA_CREATION = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testInlineSchemaCreation/api-test.yml")
      .apiPackage("net.coru.multifileplugin.inlineschemacreation")
      .modelPackage("net.coru.multifileplugin.inlineschemacreation.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(false)
      .build()
  );

  static final List<FileSpec> TEST_PARAMETER_WITH_CONTENT_GENERATION = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testApiParametersWithContentGeneration/api-test.yml")
      .apiPackage("net.coru.multifileplugin.parameterwithcontent")
      .modelPackage("net.coru.multifileplugin.parameterwithcontent.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(false)
      .build()
  );

  static final List<FileSpec> TEST_PATH_WITH_SLASH_GENERATION = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testApiPathWithSpecialCharGeneration/api-test.yml")
      .apiPackage("net.coru.multifileplugin.pathwithspecialchar")
      .modelPackage("net.coru.multifileplugin.pathwithspecialchar.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(false)
      .build()
  );

  static final List<FileSpec> TEST_API_REACTIVE_GENERATION = List.of(
    FileSpec
      .builder()
      .filePath("openapigenerator/testApiReactiveGeneration/api-test.yml")
      .apiPackage("net.coru.multifileplugin.reactivegeneration")
      .modelPackage("net.coru.multifileplugin.reactivegeneration.model")
      .modelNamePrefix("Api")
      .modelNameSuffix("DTO")
      .useLombokModelAnnotation(false)
      .isReactive(true)
      .build()
  );

  static Function<Path, Boolean> VALIDATE_ALL_OF() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testapi";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testapi/model/exception";

    final List<String> expectedTestApiFile = List.of(
      "openapigenerator/testAllOf/assets/testApi/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testAllOf/assets/testApi/ApiErrorDTO.java",
      "openapigenerator/testAllOf/assets/testApi/ApiTestAllOfDTO.java",
      "openapigenerator/testAllOf/assets/testApi/ApiTestDTO.java",
      "openapigenerator/testAllOf/assets/testApi/ApiTestInfoDTO.java",
      "openapigenerator/testAllOf/assets/testApi/ApiTestsDTO.java"

      );

    final List<String> expectedExceptionFiles = List.of(
      "openapigenerator/testAllOf/assets/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> VALIDATE_ALL_OF_LOMBOK() {


    final String DEFAULT_LOMBOK_TARGET_API = "generated/net/coru/multifileplugin/lombok/testapi";

    final String DEFAULT_LOMBOK_MODEL_API = "generated/net/coru/multifileplugin/lombok/testapi/model";

    List<String> expectedTestApiFile = List.of(
      "openapigenerator/testAllOf/assets/lombok/TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testAllOf/assets/lombok/ApiErrorDTO.java",
      "openapigenerator/testAllOf/assets/lombok/ApiTestAllOfDTO.java",
      "openapigenerator/testAllOf/assets/lombok/ApiTestDTO.java",
      "openapigenerator/testAllOf/assets/lombok/ApiTestInfoDTO.java",
      "openapigenerator/testAllOf/assets/lombok/ApiTestsDTO.java"

    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_LOMBOK_TARGET_API, DEFAULT_LOMBOK_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> VALIDATE_OVERWRITE_MODE_TRUE() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testoverwriteapi";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testoverwriteapi/model";

    final String DEFAULT_EXCEPTION_API = "generated/net/coru/multifileplugin/testoverwriteapi/model/exception";

    final List<String> expectedTestApiFile = List.of(
      "openapigenerator/testOverWriteModelTrue/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testOverWriteModelTrue/assets/ApiErrorDTO.java",
      "openapigenerator/testOverWriteModelTrue/assets/ApiTestDTO.java",
      "openapigenerator/testOverWriteModelTrue/assets/ApiTestInfoDTO.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      "openapigenerator/testOverWriteModelTrue/assets/exception/ModelClassException.java");

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, expectedExceptionFiles, DEFAULT_EXCEPTION_API);

  }

  static Function<Path, Boolean> VALIDATE_API_CLIENT_GENERATION() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/testapiclient";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/testapiclient/model";

    List<String> expectedTestApiFile = List.of(
      "openapigenerator/testApiClientGeneration/assets/TestApi.java");

    List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testApiClientGeneration/assets/ApiErrorDTO.java",
      "openapigenerator/testApiClientGeneration/assets/ApiTestDTO.java",
      "openapigenerator/testApiClientGeneration/assets/ApiTestInfoDTO.java",
      "openapigenerator/testApiClientGeneration/assets/ApiTestsDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  static Function<Path, Boolean> VALIDATE_INLINE_SCHEMA_CREATION() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/inlineschemacreation";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/inlineschemacreation/model";

    final List<String> expectedTestApiFile = List.of(
      "openapigenerator/testInlineSchemaCreation/assets/RuleApi.java",
      "openapigenerator/testInlineSchemaCreation/assets/RulesApi.java"
    );

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testInlineSchemaCreation/assets/ApiErrorDTO.java",
      "openapigenerator/testInlineSchemaCreation/assets/ApiTestDTO.java",
      "openapigenerator/testInlineSchemaCreation/assets/ApiTestProcessorDTO.java",
      "openapigenerator/testInlineSchemaCreation/assets/ApiTestTypeDTO.java"

    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> VALIDATE_PARAMETERS_WITH_SCHEMA_GENERATION() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/parameterwithcontent";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/parameterwithcontent/model";

    final List<String> expectedTestApiFile = List.of(
      "openapigenerator/testApiParametersWithContentGeneration/assets/TestApi.java");

    final List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testApiParametersWithContentGeneration/assets/ApiErrorDTO.java",
      "openapigenerator/testApiParametersWithContentGeneration/assets/ApiInlineParameterShowTestByIdTestIdDTO.java",
      "openapigenerator/testApiParametersWithContentGeneration/assets/ApiTestDTO.java",
      "openapigenerator/testApiParametersWithContentGeneration/assets/ApiTestInfoDTO.java",
      "openapigenerator/testApiParametersWithContentGeneration/assets/ApiTestsDTO.java"
      );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);
  }

  static Function<Path, Boolean> VALIDATE_PATH_WITH_SPECIAL_CHAR_GENERATION() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/pathwithspecialchar";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/pathwithspecialchar/model";

    List<String> expectedTestApiFile = List.of(
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/TestApi.java",
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/TestSchemaApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiErrorDTO.java",
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiTestDTO.java",
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiTestInfoDTO.java",
      "openapigenerator/testApiPathWithSpecialCharGeneration/assets/ApiTestsDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }


  static Function<Path, Boolean> VALIDATE_API_REACTIVE_GENERATION() {

    final String DEFAULT_TARGET_API = "generated/net/coru/multifileplugin/reactivegeneration";

    final String DEFAULT_MODEL_API = "generated/net/coru/multifileplugin/reactivegeneration/model";

    List<String> expectedTestApiFile = List.of(
      "openapigenerator/testApiReactiveGeneration/assets/TestApi.java"
    );

    List<String> expectedTestApiModelFiles = List.of(
      "openapigenerator/testApiReactiveGeneration/assets/ApiErrorDTO.java",
      "openapigenerator/testApiReactiveGeneration/assets/ApiTestDTO.java",
      "openapigenerator/testApiReactiveGeneration/assets/ApiTestInfoDTO.java",
      "openapigenerator/testApiReactiveGeneration/assets/ApiTestsDTO.java"
    );

    return (path) -> commonTest(path, expectedTestApiFile, expectedTestApiModelFiles, DEFAULT_TARGET_API, DEFAULT_MODEL_API, Collections.emptyList(), null);

  }

  private static Boolean commonTest(
    final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetApi, final String targetModel,
    final List<String> expectedExceptionFiles, final String targetException) {
    Boolean result = Boolean.TRUE;
    try {
      Path pathToTarget = Path.of(resultPath.toUri().getPath(), "target");
      Path pathToTargetApi = pathToTarget.resolve(targetApi);
      Path pathToTargetModel = pathToTarget.resolve(targetModel);

      File targetApiFolder = pathToTargetApi.toFile();
      assertThat(targetApiFolder).isNotEmptyDirectory();

      File targetModelFolder = pathToTargetModel.toFile();
      assertThat(targetModelFolder).isNotEmptyDirectory();

      validateFiles(expectedFile, targetApiFolder);
      if (!expectedModelFiles.isEmpty()) {
        validateFiles(expectedModelFiles, targetModelFolder);
      }

      if (CollectionUtils.isNotEmpty(expectedExceptionFiles)) {
        Path pathToTargetException = pathToTarget.resolve(targetException);
        File targetModelException = pathToTargetException.toFile();
        assertThat(targetModelException).isNotEmptyDirectory();
        validateFiles(expectedExceptionFiles, targetModelException);
      }
    } catch (IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  private OpenApiGeneratorFixtures() {
  }
}
