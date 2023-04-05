/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;

public class AsyncApiGeneratorFixtures {

  final static List<SpecFile> TEST_FILE_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGeneration/event-api.yml")
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

  final static List<SpecFile> TEST_ISSUE_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueGeneration/event-api.yml")
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

  final static List<SpecFile> TEST_CUSTOM_VALIDATORS = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testCustomValidators/event-api.yml")
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

  final static List<SpecFile> TEST_FILE_GENERATION_ISSUE = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationIssue/event-api.yml")
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

  final static List<SpecFile> TEST_FILE_GENERATION_EXTERNAL_AVRO = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationExternalAvro/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("subscribeOperationExternalAvro")
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

  final static List<SpecFile> TEST_FILE_GENERATION_STREAM_BRIDGE = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationStreamBridge/event-api.yml")
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

  final static List<SpecFile> TEST_FILE_GENERATION_WITHOUT_IDS = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationWithoutOperationIds/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.withoutids.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.withoutids.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .apiPackage("com.sngular.scsplugin.withoutids.model.event.producer")
                                                .modelPackage("com.sngular.scsplugin.withoutids.model.event")
                                                .modelNameSuffix("Mapper")
                                                .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_WITH_ARRAY_STRING = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationArrayString/event-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.arraywithstring.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.arraywithstring.model.event")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static List<SpecFile> TEST_ISSUE_GENERATE_SUPPLIER = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueGenerateSupplier/async-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("company.mail.messaging")
                                            .modelPackage("company.mail.model")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static List<SpecFile> TEST_ISSUE_INFINITE_LOOP = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueInfiniteLoop/async-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.infiniteLoop.messaging")
                                            .modelPackage("com.sngular.scsplugin.infiniteLoop.model")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static String TARGET = "target";

  final static String GENERATED = "generated/";

  static Function<Path, Boolean> validateTestFileGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/messages";

    final String DEFAULT_CUSTOMVALIDATOR_FOLDER = DEFAULT_MODEL_MESSAGE_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testFileGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final List<String> expectedConsumerFiles = List.of(
        ASSETS_PATH + "IPublishOperation.java",
        ASSETS_PATH + "TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
        ASSETS_PATH + "ISubscribeOperation.java",
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

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "OrderCreatedDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOMVALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/messages";

    final String COMMON_PATH = "asyncapigenerator/testIssueGeneration/";

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

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "DataClientDTO.java",
        ASSETS_PATH + "StatusDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER);
  }

  static Function<Path, Boolean> validateCustomValidators() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/producer";

    final String DEFAULT_CUSTOMVALIDATOR_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/messages/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testCustomValidators/";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/messages";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

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

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "DataClientDTO.java",
        ASSETS_PATH + "StatusDTO.java"
    );

    final List<String> expectedValidatorFiles = List.of(
        CUSTOM_VALIDATOR_PATH + "Max.java",
        CUSTOM_VALIDATOR_PATH + "MaxItems.java",
        CUSTOM_VALIDATOR_PATH + "MaxItemsValidator.java",
        CUSTOM_VALIDATOR_PATH + "MaxValidator.java",
        CUSTOM_VALIDATOR_PATH + "Min.java",
        CUSTOM_VALIDATOR_PATH + "MinItems.java",
        CUSTOM_VALIDATOR_PATH + "MinItemsValidator.java",
        CUSTOM_VALIDATOR_PATH + "MinValidator.java",
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

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOMVALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationIssue() {

    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/messages";

    final String DEFAULT_CUSTOMVALIDATOR_FOLDER = DEFAULT_MODEL_MESSAGE_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationIssue/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

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

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "CustomerEventMessageDTO.java",
        ASSETS_PATH + "CustomerOrderEventMessageDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOMVALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationExternalAvro() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationExternalAvro/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
        ASSETS_PATH + "ISubscribeOperation.java",
        ASSETS_PATH + "Subscriber.java"
    );

    final List<String> expectedProducerFiles = List.of(
        ASSETS_PATH + "IPublishOperation.java",
        ASSETS_PATH + "Producer.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationStreamBridge() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationStreamBridge/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
        ASSETS_PATH + "ISubscribeOperation.java",
        ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
        ASSETS_PATH + "StreamBridgeProducer.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithoutIds() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationWithoutOperationIds/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedConsumerFiles = List.of(
    ASSETS_PATH + "ISubscribeOperation.java",
        ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
        ASSETS_PATH + "StreamBridgeProducer.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationArrayString() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/arraywithstring/model/event/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/arraywithstring/model/event/messages";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationArrayString/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedModelSchemaFiles = List.of(
        ASSETS_PATH + "ObjectArrayDTO.java"
    );

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "ObjectArrayMessageDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGenerateSupplier() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/company/mail/model/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/company/mail/model/messages";

    final List<String> expectedModelSchemaFiles = List.of(
        "asyncapigenerator/testIssueGenerateSupplier/assets/ConfigurationDTO.java",
        "asyncapigenerator/testIssueGenerateSupplier/assets/MailRequestDTO.java"
    );

    final List<String> expectedModelMessageFiles = List.of(
        "asyncapigenerator/testIssueGenerateSupplier/assets/MailRequestMessageDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueInfiniteLoop() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/infiniteLoop/model/schemas";

    final String DEFAULT_MODEL_MESSAGE_FOLDER = "generated/com/sngular/scsplugin/infiniteLoop/model/messages";

    final List<String> expectedModelSchemaFiles = List.of(
        "asyncapigenerator/testIssueInfiniteLoop/assets/ConfigDTO.java",
        "asyncapigenerator/testIssueInfiniteLoop/assets/MailRequestInfiniteDTO.java"
    );

    final List<String> expectedModelMessageFiles = List.of(
        "asyncapigenerator/testIssueInfiniteLoop/assets/duplicateNameMessage/MailRequestInfiniteDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedModelMessageFiles, DEFAULT_MODEL_MESSAGE_FOLDER);
  }

  private static Boolean commonTest(
      final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetConsumer, final String targetProducer) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");
      final Path pathToTargetConsumer = pathToTarget.resolve(targetConsumer);

      final File targetConsumerFolder = pathToTargetConsumer.toFile();
      assertThat(targetConsumerFolder).isNotEmptyDirectory();
      TestUtils.validateFiles(expectedFile, targetConsumerFolder);

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetProducer = pathToTarget.resolve(targetProducer);
        final File targetProducerFolder = pathToTargetProducer.toFile();
        assertThat(targetProducerFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetProducerFolder);
      }
    } catch (final IOException e) {
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
        assertThat(targetModelFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetModelFolder);
      }
    } catch (final IOException e) {
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
        assertThat(targetCustomValidatorFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedValidatorFiles, targetCustomValidatorFolder);
      }
    } catch (final IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }
}
