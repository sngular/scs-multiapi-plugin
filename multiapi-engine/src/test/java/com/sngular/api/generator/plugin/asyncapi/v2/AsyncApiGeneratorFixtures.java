/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.v2;

import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.assertj.core.api.Assertions;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static java.util.Collections.singletonList;

public class AsyncApiGeneratorFixtures {

  static final List<SpecFile> TEST_FILE_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("publishOperationFileGeneration")
        .classNamePostfix("TestClassName")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.filegeneration.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.filegeneration.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("subscribeOperationFileGeneration")
        .modelNameSuffix("Mapper")
        .apiPackage("com.sngular.scsplugin.filegeneration.model.event.producer")
        .modelPackage("com.sngular.scsplugin.filegeneration.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_NO_CONFIG = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationNoConfiguration/event-api.yml")
      .build()
  );

  static final List<SpecFile> TEST_ISSUE_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testIssueGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("response")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.issuegeneration.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.issuegeneration.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("clients")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.issuegeneration.model.event.producer")
        .modelPackage("com.sngular.scsplugin.issuegeneration.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_ISSUE_SIMPLE_TYPE_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testIssueSimpleTypeGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("response")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.issuesimpletypegeneration.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.issuesimpletypegeneration.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("clients")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.issuesimpletypegeneration.model.event.producer")
        .modelPackage("com.sngular.scsplugin.issuesimpletypegeneration.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_RESERVED_WORDS_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testReservedWordsGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOperationFileGeneration")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.reservedwordsgeneration.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.reservedwordsgeneration.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("publishOperationFileGeneration")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.reservedwordsgeneration.model.event.producer")
        .modelPackage("com.sngular.scsplugin.reservedwordsgeneration.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_RARE_CHARS_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testRareCharsGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOperationFileGeneration")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.rarecharsgeneration.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.rarecharsgeneration.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("publishOperationFileGeneration")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.rarecharsgeneration.model.event.producer")
        .modelPackage("com.sngular.scsplugin.rarecharsgeneration.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_CUSTOM_VALIDATORS = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testCustomValidators/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("customValidatorResponse")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.customvalidator.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.customvalidator.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("customValidatorClients")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.customvalidator.model.event.producer")
        .modelPackage("com.sngular.scsplugin.customvalidator.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_ISSUE = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationIssue/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("onCustomerEvent")
        .classNamePostfix("TestClassName")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.filegenerationissue.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.filegenerationissue.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("onCustomerOrderEvent")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.filegenerationissue.model.event.producer")
        .modelPackage("com.sngular.scsplugin.filegenerationissue.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_EXTERNAL_AVRO = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationExternalAvro/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOperationExternalAvro,subscribeReceiptExternalAvro")
        .apiPackage("com.sngular.scsplugin.externalavro.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.externalavro.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("publishOperationExternalAvro")
        .apiPackage("com.sngular.scsplugin.externalavro.model.event.producer")
        .modelPackage("com.sngular.scsplugin.externalavro.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_ISSUE_INVALID_AVRO = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testIssueInvalidAvro/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOperationExternalAvro")
        .apiPackage("com.sngular.scsplugin.issueAvro.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.issueAvro.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("publishOperationExternalAvro")
        .apiPackage("com.sngular.scsplugin.issueAvro.model.event.producer")
        .modelPackage("com.sngular.scsplugin.issueAvro.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_STREAM_BRIDGE = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationStreamBridge/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOperationStreamBridge")
        .classNamePostfix("TestClassName")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.streambridge.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.streambridge.model.event")
        .build())
      .streamBridge(OperationParameterObject.builder()
        .ids("publishOperationStreamBridge")
        .apiPackage("com.sngular.scsplugin.streambridge.model.event.producer")
        .modelPackage("com.sngular.scsplugin.streambridge.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_WITHOUT_OPERATION_IDS = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationWithoutOperationIds/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .classNamePostfix("TestClassName")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.withoutoperationids.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.withoutoperationids.model.event")
        .build())
      .streamBridge(OperationParameterObject.builder()
        .apiPackage("com.sngular.scsplugin.withoutoperationids.model.event.producer")
        .modelPackage("com.sngular.scsplugin.withoutoperationids.model.event")
        .modelNameSuffix("DTO")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_FILE_GENERATION_WITH_ARRAY_STRING = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationArrayString/event-api.yml")
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.arraywithstring.model.event.producer")
        .modelPackage("com.sngular.scsplugin.arraywithstring.model.event")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_ISSUE_GENERATE_SUPPLIER = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testIssueGenerateSupplier/event-api.yml")
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("DTO")
        .apiPackage("company.mail.messaging")
        .modelPackage("company.mail.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_ISSUE_INFINITE_LOOP = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testIssueInfiniteLoop/event-api.yml")
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.infiniteLoop.messaging")
        .modelPackage("com.sngular.scsplugin.infiniteLoop.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_MODEL_CLASS_EXCEPTION_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testModelClassExceptionGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("subscribeOrder")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.modelclass.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.modelclass.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("publishOrder")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.modelclass.model.event.producer")
        .modelPackage("com.sngular.scsplugin.modelclass.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_GENERATION_WITH_NO_OPERATION_ID = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testGenerationWithNoOperationId/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.withoutoperationid.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.withoutoperationid.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_NO_SCHEMAS = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testNoSchemas/event-api.yml")
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.noschemas")
        .modelPackage("com.sngular.scsplugin.noschemas.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_MESSAGE_NAMING = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testMessageNaming/event-api.yml")
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.messagenaming")
        .modelPackage("com.sngular.scsplugin.messagenaming.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_NESTED_OBJECT_ISSUE = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testNestedObjectIssue/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.nestedobject.consumer")
        .modelPackage("com.sngular.scsplugin.nestedobject.model")
        .useLombokModelAnnotation(true)
        .build())
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.nestedobject.producer")
        .modelPackage("com.sngular.scsplugin.nestedobject.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_CONSTANT_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testConstantGeneration/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.constantgeneration.consumer")
        .modelPackage("com.sngular.scsplugin.constantgeneration.model")
        .useLombokModelAnnotation(true)
        .build())
      .supplier(OperationParameterObject.builder()
        .modelNameSuffix("")
        .apiPackage("com.sngular.scsplugin.constantgeneration.producer")
        .modelPackage("com.sngular.scsplugin.constantgeneration.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> PROPERTIES_NOT_GENERATED_ISSUE = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testPropertiesNotGeneratedIssue/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .modelNameSuffix("")
        .operationIds(List.of("emitUserSignUpEvent"))
        .apiPackage("com.sngular.scsplugin.notgeneratedproperties.consumer")
        .modelPackage("com.sngular.scsplugin.notgeneratedproperties.model")
        .useLombokModelAnnotation(true)
        .build())
      .build());

  static final List<SpecFile> TEST_FILE_GENERATION_WITH_KAFKA_BINDINGS = List.of(
    SpecFile
      .builder()
      .filePath("asyncapigenerator/v2/testFileGenerationWithKafkaBindings/event-api.yml")
      .consumer(OperationParameterObject.builder()
        .ids("publishOperationFileGenerationWithKafkaBindings")
        .classNamePostfix("TestClassName")
        .modelNameSuffix("DTO")
        .apiPackage("com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.consumer")
        .modelPackage("com.sngular.scsplugin.filegenerationwithkafkabindings.model.event")
        .build())
      .supplier(OperationParameterObject.builder()
        .ids("subscribeOperationFileGenerationWithKafkaBindings")
        .modelNameSuffix("Mapper")
        .apiPackage("com.sngular.scsplugin.filegenerationwithkafkabindings.model.event.producer")
        .modelPackage("com.sngular.scsplugin.filegenerationwithkafkabindings.model.event")
        .build())
      .build()
  );

  static final List<SpecFile> TEST_SUB_OBJECT_SAME_NAME =
    List.of(
      SpecFile.builder()
        .filePath("asyncapigenerator/v2/testSubObjectSameName/event-api.yml")
        .consumer(
          OperationParameterObject.builder()
            .ids("input")
            .apiPackage("input.controller")
            .modelPackage("input.model")
            .build())
        .supplier(
          OperationParameterObject.builder()
            .ids("output")
            .apiPackage("output.provider")
            .modelPackage("output.model")
            .build())
        .build());

  static final List<SpecFile> TEST_REFERENCE_FROM_LOCAL_ISSUE =
    List.of(
      SpecFile.builder()
        .filePath("asyncapigenerator/v2/testReferenceFromLocalIssue/event-api.yml")
        .consumer(
          OperationParameterObject.builder()
            .ids("userSignedUp")
            .apiPackage("com.github.issue.listener")
            .modelPackage("com.github.issue.model")
            .build())
        .build());

  static final String TARGET = "target";

  static final String GENERATED = "generated/";

  static Function<Path, Boolean> validateTestFileGeneration() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final String DEFAULT_EXCEPTION_API = DEFAULT_COMMON_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "IPublishOperationFileGeneration.java",
      ASSETS_PATH + "TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationFileGeneration.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CreateOrderMapper.java",
      ASSETS_PATH + "OrderDTO.java",
      ASSETS_PATH + "OrderLineDTO.java",
      ASSETS_PATH + "OrderLineMapper.java",
      ASSETS_PATH + "OrderMapper.java",
      ASSETS_PATH + "OrderProductDTO.java",
      ASSETS_PATH + "OrderProductMapper.java",
      ASSETS_PATH + "WaiterMapper.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      CUSTOM_VALIDATOR_PATH + "NotNull.java",
      CUSTOM_VALIDATOR_PATH + "NotNullValidator.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_COMMON_FOLDER) &&
      customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  private static Boolean commonTest(
    final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetConsumer,
    final String targetProducer, final List<String> expectedExceptionFiles, final String targetException) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");
      final Path pathToTargetConsumer = pathToTarget.resolve(targetConsumer);

      final File targetConsumerFolder = pathToTargetConsumer.toFile();
      Assertions.assertThat(targetConsumerFolder).isNotEmptyDirectory();
      TestUtils.validateFiles(expectedFile, targetConsumerFolder);

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetProducer = pathToTarget.resolve(targetProducer);
        final File targetProducerFolder = pathToTargetProducer.toFile();
        Assertions.assertThat(targetProducerFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetProducerFolder);
      }

      if (CollectionUtils.isNotEmpty(expectedExceptionFiles)) {
        final Path pathToTargetException = pathToTarget.resolve(targetException);
        final File targetModelException = pathToTargetException.toFile();
        Assertions.assertThat(targetModelException).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedExceptionFiles, targetModelException);
      }
    } catch (final URISyntaxException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  private static boolean modelTest(final Path resultPath, final List<String> expectedModelFiles, final String default_model_folder) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetModel = pathToTarget.resolve(default_model_folder);
        final File targetModelFolder = pathToTargetModel.toFile();
        Assertions.assertThat(targetModelFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetModelFolder);
      }
    } catch (final URISyntaxException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  private static boolean customValidatorTest(final Path resultPath, final List<String> expectedValidatorFiles, final String default_customvalidator_folder) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");

      if (!expectedValidatorFiles.isEmpty()) {
        final Path pathToTargetCustomValidator = pathToTarget.resolve(default_customvalidator_folder);
        final File targetCustomValidatorFolder = pathToTargetCustomValidator.toFile();
        Assertions.assertThat(targetCustomValidatorFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedValidatorFiles, targetCustomValidatorFolder);
      }
    } catch (final URISyntaxException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  static Function<Path, Boolean> validateTestIssueGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event";

    final String COMMON_PATH = "asyncapigenerator/v2/testIssueGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "IResponse.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IClients.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "DataDTO.java",
      ASSETS_PATH + "StatusMsgDTO.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      Collections.emptyList(), null) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueSimpleTypeGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/issuesimpletypegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/issuesimpletypegeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/issuesimpletypegeneration/model/event";

    final String COMMON_PATH = "asyncapigenerator/v2/testIssueSimpleTypeGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "IResponse.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IClients.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "DataDTO.java",
      ASSETS_PATH + "StatusMsgDTO.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      Collections.emptyList(), null) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestReservedWordsGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/reservedwordsgeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/reservedwordsgeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/reservedwordsgeneration/model/event";

    final String COMMON_PATH = "asyncapigenerator/v2/testReservedWordsGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationFileGeneration.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IPublishOperationFileGeneration.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CreateOrderDTO.java",
      ASSETS_PATH + "OrderDTO.java",
      ASSETS_PATH + "OrderLineDTO.java",
      ASSETS_PATH + "WaiterDTO.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER, Collections.emptyList(), null) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestRareCharsGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/rarecharsgeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/rarecharsgeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/rarecharsgeneration/model/event";

    final String COMMON_PATH = "asyncapigenerator/v2/testRareCharsGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationFileGeneration.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IPublishOperationFileGeneration.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CreateOrderDTO.java",
      ASSETS_PATH + "OrderDTO.java",
      ASSETS_PATH + "WaiterDTO.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER, Collections.emptyList(), null) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateCustomValidators(final int springBootVersion) {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/v2/testCustomValidators/";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/" + calculateJavaEEPackage(springBootVersion);

    final String DEFAULT_EXCEPTION_API = DEFAULT_MODEL_SCHEMA_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ICustomValidatorResponse.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "ICustomValidatorClients.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "DataDTO.java",
      ASSETS_PATH + "StatusMsgDTO.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      CUSTOM_VALIDATOR_PATH + "MaxBigDecimal.java",
      CUSTOM_VALIDATOR_PATH + "MaxBigDecimalValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxDouble.java",
      CUSTOM_VALIDATOR_PATH + "MaxDoubleValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxFloat.java",
      CUSTOM_VALIDATOR_PATH + "MaxFloatValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxInteger.java",
      CUSTOM_VALIDATOR_PATH + "MaxIntegerValidator.java",
      CUSTOM_VALIDATOR_PATH + "MaxItems.java",
      CUSTOM_VALIDATOR_PATH + "MaxItemsValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinBigDecimal.java",
      CUSTOM_VALIDATOR_PATH + "MinBigDecimalValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinDouble.java",
      CUSTOM_VALIDATOR_PATH + "MinDoubleValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinFloat.java",
      CUSTOM_VALIDATOR_PATH + "MinFloatValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinInteger.java",
      CUSTOM_VALIDATOR_PATH + "MinIntegerValidator.java",
      CUSTOM_VALIDATOR_PATH + "MinItems.java",
      CUSTOM_VALIDATOR_PATH + "MinItemsValidator.java",
      CUSTOM_VALIDATOR_PATH + "MultipleOf.java",
      CUSTOM_VALIDATOR_PATH + "MultipleOfValidator.java",
      CUSTOM_VALIDATOR_PATH + "NotNull.java",
      CUSTOM_VALIDATOR_PATH + "NotNullValidator.java",
      CUSTOM_VALIDATOR_PATH + "Pattern.java",
      CUSTOM_VALIDATOR_PATH + "PatternValidator.java",
      CUSTOM_VALIDATOR_PATH + "Size.java",
      CUSTOM_VALIDATOR_PATH + "SizeValidator.java",
      CUSTOM_VALIDATOR_PATH + "UniqueItems.java",
      CUSTOM_VALIDATOR_PATH + "UniqueItemsValidator.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
      customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  private static String calculateJavaEEPackage(final int springBootVersion) {
    if (3 <= springBootVersion) {
      return "jakarta/";
    } else {
      return "javax/";
    }
  }

  static Function<Path, Boolean> validateTestFileGenerationIssue() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationIssue/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final String DEFAULT_EXCEPTION_API = DEFAULT_COMMON_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "IOnCustomerEvent.java",
      ASSETS_PATH + "TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IOnCustomerOrderEvent.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CustomerDTO.java",
      ASSETS_PATH + "CustomerEventPayloadDTO.java",
      ASSETS_PATH + "CustomerOrderDTO.java",
      ASSETS_PATH + "CustomerOrderEventPayloadDTO.java",
      ASSETS_PATH + "OrderedItemDTO.java",
      ASSETS_PATH + "PaymentDetailsDTO.java",
      ASSETS_PATH + "ShippingDetailsDTO.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      CUSTOM_VALIDATOR_PATH + "NotNull.java",
      CUSTOM_VALIDATOR_PATH + "NotNullValidator.java",
      CUSTOM_VALIDATOR_PATH + "Size.java",
      CUSTOM_VALIDATOR_PATH + "SizeValidator.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_COMMON_FOLDER) &&
      customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationExternalAvro() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationExternalAvro/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationExternalAvro.java",
      ASSETS_PATH + "ISubscribeReceiptExternalAvro.java",
      ASSETS_PATH + "Subscriber.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IPublishOperation.java",
      ASSETS_PATH + "Producer.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      Collections.emptyList(), null);
  }

  static Function<Path, Boolean> validateTestFileGenerationStreamBridge() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationStreamBridge/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String DEFAULT_EXCEPTION_API = DEFAULT_COMMON_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationStreamBridge.java",
      ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "StreamBridgeProducer.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithoutOperationIds() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/withoutoperationids/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationWithoutOperationIds/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String DEFAULT_EXCEPTION_API = DEFAULT_COMMON_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperation.java",
      ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "StreamBridgeProducer.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateTestFileGenerationArrayString() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/arraywithstring/model/event";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationArrayString/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "ObjectArrayDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGenerateSupplier() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/company/mail/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testIssueGenerateSupplier/assets/ConfigurationDTO.java",
      "asyncapigenerator/v2/testIssueGenerateSupplier/assets/MailRequestDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueInfiniteLoop() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/infiniteLoop/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testIssueInfiniteLoop/assets/ConfigDTO.java",
      "asyncapigenerator/v2/testIssueInfiniteLoop/assets/MailRequestInfiniteDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestModelClassExceptionGeneration() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/modelclass/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/v2/testModelClassExceptionGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final String DEFAULT_EXCEPTION_API = DEFAULT_COMMON_FOLDER + "/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOrder.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IPublishOrder.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CreateOrderEventDTO.java",
      ASSETS_PATH + "OrderDTO.java",
      ASSETS_PATH + "OrderLineDTO.java",
      ASSETS_PATH + "OrderProductDTO.java",
      ASSETS_PATH + "WaiterDTO.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      CUSTOM_VALIDATOR_PATH + "NotNull.java",
      CUSTOM_VALIDATOR_PATH + "NotNullValidator.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
      expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_COMMON_FOLDER) &&
      customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateNoSchemas() {
    final String API_FOLDER = "generated/com/sngular/scsplugin/noschemas";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/noschemas/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testNoSchemas/assets/TestMsg.java",
      "asyncapigenerator/v2/testNoSchemas/assets/Thing.java"
    );

    final List<String> expectedProducerFiles = List.of(
      "asyncapigenerator/v2/testNoSchemas/assets/IOnTest.java",
      "asyncapigenerator/v2/testNoSchemas/assets/IOnTest2.java",
      "asyncapigenerator/v2/testNoSchemas/assets/Producer.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
      modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateMessageNaming() {
    final String API_FOLDER = "generated/com/sngular/scsplugin/messagenaming";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/messagenaming/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testMessageNaming/assets/OnTest3.java",
      "asyncapigenerator/v2/testMessageNaming/assets/TestMsg.java",
      "asyncapigenerator/v2/testMessageNaming/assets/TestMsg2.java"
    );

    final List<String> expectedProducerFiles = List.of(
      "asyncapigenerator/v2/testMessageNaming/assets/IOnTest.java",
      "asyncapigenerator/v2/testMessageNaming/assets/IOnTest2.java",
      "asyncapigenerator/v2/testMessageNaming/assets/IOnTest3.java",
      "asyncapigenerator/v2/testMessageNaming/assets/Producer.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
      modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateNestedObject() {
    final String API_FOLDER = "generated/com/sngular/scsplugin/nestedobject";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/nestedobject/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testNestedObjectIssue/assets/payload/SomeOtherObject.java",
      "asyncapigenerator/v2/testNestedObjectIssue/assets/payload/UserSignedUpPayload.java"
    );

    final List<String> expectedProducerFiles = List.of();

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
      modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateConstantGeneration() {
    final String API_FOLDER = "generated/com/sngular/scsplugin/constantgeneration";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/constantgeneration/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testConstantGeneration/assets/payload/SomeOtherObject.java",
      "asyncapigenerator/v2/testConstantGeneration/assets/payload/UserSignedUpPayload.java"
    );

    final List<String> expectedProducerFiles = List.of();

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
      modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateNotGeneratedPropertiesIssue() {
    final String API_FOLDER = "generated/com/sngular/scsplugin/notgeneratedproperties";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/notgeneratedproperties/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/v2/testPropertiesNotGeneratedIssue/assets/payload/UserDetails.java",
      "asyncapigenerator/v2/testPropertiesNotGeneratedIssue/assets/payload/UserSignedUp.java"
    );

    final List<String> expectedProducerFiles = List.of();

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
      modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithKafkaBindings() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegenerationwithkafkabindings/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/v2/testFileGenerationWithKafkaBindings/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "IPublishOperationFileGenerationWithKafkaBindings.java",
      ASSETS_PATH + "consumer/MessageWrapper.java",
      ASSETS_PATH + "TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationFileGenerationWithKafkaBindings.java",
      ASSETS_PATH + "producer/MessageWrapper.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "CreateOrderMapper.java",
      ASSETS_PATH + "OrderDTO.java",
      ASSETS_PATH + "OrderLineDTO.java",
      ASSETS_PATH + "OrderLineMapper.java",
      ASSETS_PATH + "OrderMapper.java",
      ASSETS_PATH + "OrderProductDTO.java",
      ASSETS_PATH + "OrderProductMapper.java",
      ASSETS_PATH + "WaiterMapper.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      CUSTOM_VALIDATOR_PATH + "NotNull.java",
      CUSTOM_VALIDATOR_PATH + "NotNullValidator.java"
    );

    return path -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER, Collections.emptyList(), null) &&
      modelTest(path, expectedModelSchemaFiles, DEFAULT_COMMON_FOLDER) &&
      customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestSubObjectSameName() {

    final String DEFAULT_COMMON_FOLDER = "generated";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/input/controller";

    final String DEFAULT_CONSUMER_MODEL_FOLDER = DEFAULT_COMMON_FOLDER + "/input/model";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/output/provider";

    final String DEFAULT_PRODUCER_MODEL_FOLDER = DEFAULT_COMMON_FOLDER + "/output/model";

    final String COMMON_PATH = "asyncapigenerator/v2/testSubObjectSameName/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles =
      List.of(
        ASSETS_PATH + "input/controller/IInput.java",
        ASSETS_PATH + "input/controller/Subscriber.java");

    final List<String> expectedProducerFiles =
      List.of(
        ASSETS_PATH + "output/provider/IOutput.java",
        ASSETS_PATH + "output/provider/Producer.java");

    final List<String> expectedConsumerModelSchemaFiles =
      List.of(ASSETS_PATH + "input/model/Data.java", ASSETS_PATH + "input/model/Input.java");
    final List<String> expectedProducerModelSchemaFiles =
      List.of(ASSETS_PATH + "output/model/Data.java", ASSETS_PATH + "output/model/Output.java");

    return path ->
      commonTest(
        path,
        expectedConsumerFiles,
        expectedProducerFiles,
        DEFAULT_CONSUMER_FOLDER,
        DEFAULT_PRODUCER_FOLDER,
        Collections.emptyList(),
        null)
        && modelTest(path, expectedConsumerModelSchemaFiles, DEFAULT_CONSUMER_MODEL_FOLDER)
        && modelTest(path, expectedProducerModelSchemaFiles, DEFAULT_PRODUCER_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestReferenceFromLocalIssue() {

    final String DEFAULT_COMMON_FOLDER = "generated";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/com/github/issue/listener";

    final String DEFAULT_CONSUMER_MODEL_FOLDER = DEFAULT_COMMON_FOLDER + "/com/github/issue/model";

    final String COMMON_PATH = "asyncapigenerator/v2/testReferenceFromLocalIssue/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles =
        List.of(
            ASSETS_PATH + "IUserSignedUp.java",
            ASSETS_PATH + "Subscriber.java");

    final List<String> expectedConsumerModelSchemaFiles =
        singletonList(ASSETS_PATH + "/UserMessage.java");

    return path ->
        commonTest(
            path,
            expectedConsumerFiles,
            Collections.emptyList(),
            DEFAULT_CONSUMER_FOLDER,
            null,
            Collections.emptyList(),
            null)
            && modelTest(path, expectedConsumerModelSchemaFiles, DEFAULT_CONSUMER_MODEL_FOLDER);
  }
}
