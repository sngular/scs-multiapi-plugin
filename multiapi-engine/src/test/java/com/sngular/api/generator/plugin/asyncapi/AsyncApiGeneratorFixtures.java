/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;

import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;
import org.apache.commons.collections4.CollectionUtils;

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

  final static List<SpecFile> TEST_FILE_GENERATION_NO_CONFIG = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testFileGenerationNoConfiguration/event-api.yml")
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

  final static List<SpecFile> TEST_CUSTOM_VALIDATORS_DIFFERENT_FOLDER = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testCustomValidatorsDifferentModel/event-api.yml")
      .consumer(OperationParameterObject.builder()
                                        .ids("customValidatorResponse")
                                        .modelNameSuffix("DTO")
                                        .apiPackage("com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer")
                                        .modelPackage("com.sngular.scsplugin.customvalidatordifferentmodel.event.consumer.model")
                                        .build())
      .supplier(OperationParameterObject.builder()
                                        .ids("customValidatorClients")
                                        .modelNameSuffix("DTO")
                                        .apiPackage("com.sngular.scsplugin.customvalidatordifferentmodel.event.producer")
                                        .modelPackage("com.sngular.scsplugin.customvalidatordifferentmodel.event.producer.model")
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

  final static List<SpecFile> TEST_PARAMETERIZED_CHANNEL_GENERATOR = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testParameterizedChannelGeneration/event-api.yml")
      .streamBridge(OperationParameterObject.builder()
                  .operationIds(List.of("receiveLightMeasurement"))
                  .modelNameSuffix("DTO")
                  .apiPackage("smartylighting.streetlights.messaging.producer")
                  .modelPackage("smartylighting.streetlights.messaging.producer.model")
                  .useLombokModelAnnotation(true)
                  .build())
      .consumer(OperationParameterObject.builder()
                  .operationIds(List.of("turnOn", "turnOff", "dimLight"))
                  .apiPackage("smartylighting.streetlights.messaging.consumer")
                  .modelPackage("smartylighting.streetlights.messaging.consumer.model")
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

  final static List<SpecFile> TEST_MODEL_CLASS_EXCEPTION_GENERATION = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testModelClassExceptionGeneration/event-api.yml")
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

  final static List<SpecFile> TEST_GENERATION_WITH_NO_OPERATION_ID = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testGenerationWithNoOperationId/event-api.yml")
      .consumer(OperationParameterObject.builder()
                                        .modelNameSuffix("DTO")
                                        .apiPackage("com.sngular.scsplugin.withoutoperationid.model.event.consumer")
                                        .modelPackage("com.sngular.scsplugin.withoutoperationid.model.event")
                                        .build())
      .build()
  );

  final static List<SpecFile> TEST_NO_SCHEMAS = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testNoSchemas/event-api.yml")
      .supplier(OperationParameterObject.builder()
                                        .modelNameSuffix("")
                                        .apiPackage("com.sngular.scsplugin.noschemas")
                                        .modelPackage("com.sngular.scsplugin.noschemas.model")
                                        .useLombokModelAnnotation(true)
                                        .build())
      .build());

  final static List<SpecFile> TEST_NESTED_OBJECT = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testNestedObjectIssue/event-api.yml")
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

final static List<SpecFile> PROPERTIES_NOT_GENERATED_ISSUE = List.of(
    SpecFile
      .builder()
      .filePath("src/test/resources/asyncapigenerator/testPropertiesNotGeneratedIssue/event-api.yml")
      .consumer(OperationParameterObject.builder()
                                        .modelNameSuffix("")
                                        .operationIds(List.of("emitUserSignUpEvent"))
                                        .apiPackage("com.sngular.scsplugin.notgeneratedproperties.consumer")
                                        .modelPackage("com.sngular.scsplugin.notgeneratedproperties.model")
                                        .useLombokModelAnnotation(true)
                                        .build())
      .build());

  final static List<SpecFile> TEST_FILE_GENERATION_WITH_KAFKA_BINDINGS = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationWithKafkaBindings/event-api.yml")
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

  final static String TARGET = "target";

  final static String GENERATED = "generated/";

  static Function<Path, Boolean> validateTestFileGeneration() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = DEFAULT_COMMON_FOLDER;

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testFileGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/filegeneration/model/event/exception";

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

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

   static Function<Path, Boolean> validateTestIssueGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event";

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

    final List<String> expectedExceptionFiles = List.of();

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, null) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateCustomValidators(int springBootVersion) {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testCustomValidators/";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/customvalidator/model/event";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/" + calculateJavaEEPackage(springBootVersion);

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/customvalidator/model/event/exception";

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

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateCustomValidatorsDifferentModel(int springBootVersion) {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/consumer/";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/producer/model/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testCustomValidatorsDifferentModel/";

    final String DEFAULT_CONSUMER_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/consumer/model";
    final String DEFAULT_PRODUCER_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/producer/model";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/" + calculateJavaEEPackage(springBootVersion);

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/customvalidatordifferentmodel/event/consumer/model/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ICustomValidatorResponse.java",
      ASSETS_PATH + "Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "ICustomValidatorClients.java",
      ASSETS_PATH + "Producer.java");

    final List<String> expectedConsumerModelSchemaFiles = List.of(
      ASSETS_PATH + "StatusMsgDTO.java"
    );

    final List<String> expectedProducerModelSchemaFiles = List.of(
      ASSETS_PATH + "DataDTO.java"
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

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
                     modelTest(path, expectedConsumerModelSchemaFiles, DEFAULT_CONSUMER_MODEL_SCHEMA_FOLDER) &&
                     modelTest(path, expectedProducerModelSchemaFiles, DEFAULT_PRODUCER_MODEL_SCHEMA_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationIssue() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationIssue/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String CUSTOM_VALIDATOR_PATH = COMMON_PATH + "customvalidator/";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/filegenerationissue/model/event/exception";

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

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_COMMON_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationExternalAvro() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationExternalAvro/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/externalavro/model/event/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationExternalAvro.java",
      ASSETS_PATH + "Subscriber.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "IPublishOperation.java",
      ASSETS_PATH + "Producer.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                emptyList(), DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateTestFileGenerationStreamBridge() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationStreamBridge/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/streambridge/model/event/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperationStreamBridge.java",
      ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "StreamBridgeProducer.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithoutIds() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/producer";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationWithoutOperationIds/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String DEFAULT_EXCEPTION_API = "generated/com/sngular/scsplugin/withoutids/model/event/exception";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "ISubscribeOperation.java",
      ASSETS_PATH + "TestClassName.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "StreamBridgeProducer.java"
    );

    final List<String> expectedExceptionFiles = List.of(
      ASSETS_PATH + "ModelClassException.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API);
  }

  static Function<Path, Boolean> validateTestFileGenerationArrayString() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/arraywithstring/model/event";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationArrayString/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final List<String> expectedModelSchemaFiles = List.of(
      ASSETS_PATH + "ObjectArrayDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGenerateSupplier() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/company/mail/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/testIssueGenerateSupplier/assets/ConfigurationDTO.java",
      "asyncapigenerator/testIssueGenerateSupplier/assets/MailRequestDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestParameterizedChannelGeneration() {
    final String DEFAULT_COMMON_FOLDER = "generated/smartylighting/streetlights/messaging";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String CONSUMER_MODEL_FOLDER = DEFAULT_CONSUMER_FOLDER + "/model";

    final String PRODUCER_MODEL_FOLDER = DEFAULT_PRODUCER_FOLDER + "/model";

    final String COMMON_PATH = "asyncapigenerator/testParameterizedChannelGeneration/";

    final String ASSETS_PATH = COMMON_PATH + "assets/";

    final String EXPECTED_CONSUMER_MODEL_PATH = ASSETS_PATH + "/consumer/model";

    final String EXPECTED_PRODUCER_MODEL_PATH = ASSETS_PATH + "/producer/model";

    final String CUSTOM_VALIDATOR_PRODUCER_PATH = PRODUCER_MODEL_FOLDER + "/customvalidator/";

    final List<String> expectedConsumerFiles = List.of(
      ASSETS_PATH + "consumer/IDimLight.java",
      ASSETS_PATH + "consumer/ITurnOff.java",
      ASSETS_PATH + "consumer/ITurnOn.java",
      ASSETS_PATH + "consumer/Subscriber.java"
    );

    final List<String> expectedProducerFiles = List.of(
      ASSETS_PATH + "producer/StreamBridgeProducer.java"
    );


    final List<String> expectedConsumerModelSchemaFiles = List.of(
      EXPECTED_CONSUMER_MODEL_PATH + "/DimLightPayload.java",
      EXPECTED_CONSUMER_MODEL_PATH + "/TurnOnOffPayload.java"
    );

    final List<String> expectedProducerModelSchemaFiles = List.of(
      EXPECTED_PRODUCER_MODEL_PATH + "/LightMeasuredPayloadDTO.java"
    );

    final List<String> expectedValidatorFiles = List.of(
      EXPECTED_PRODUCER_MODEL_PATH + "/customvalidator/Max.java",
      EXPECTED_PRODUCER_MODEL_PATH + "/customvalidator/MaxValidator.java",
      EXPECTED_PRODUCER_MODEL_PATH + "/customvalidator/Min.java",
      EXPECTED_PRODUCER_MODEL_PATH + "/customvalidator/MinValidator.java"
    );

    return path ->  commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                               null, null) &&
                    modelTest(path, expectedConsumerModelSchemaFiles, CONSUMER_MODEL_FOLDER) &&
                    modelTest(path, expectedProducerModelSchemaFiles, PRODUCER_MODEL_FOLDER) &&
                    customValidatorTest(path, expectedValidatorFiles, CUSTOM_VALIDATOR_PRODUCER_PATH);
  }

  static Function<Path, Boolean> validateTestIssueInfiniteLoop() {
    final String DEFAULT_MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/infiniteLoop/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/testIssueInfiniteLoop/assets/ConfigDTO.java",
      "asyncapigenerator/testIssueInfiniteLoop/assets/MailRequestInfiniteDTO.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER);
  }

  static Function<Path, Boolean> validateTestModelClassExceptionGeneration() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/modelclass/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testModelClassExceptionGeneration/";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = DEFAULT_COMMON_FOLDER ;

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

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER,
                                expectedExceptionFiles, DEFAULT_EXCEPTION_API) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
                     customValidatorTest(path, expectedValidatorFiles, DEFAULT_CUSTOM_VALIDATOR_FOLDER);
  }

  static Function<Path, Boolean> validateNoSchemas () {
    final String API_FOLDER = "generated/com/sngular/scsplugin/noschemas";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/noschemas/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/testNoSchemas/assets/TestMsg.java",
      "asyncapigenerator/testNoSchemas/assets/Thing.java"
    );

    final List<String> expectedProducerFiles = List.of(
      "asyncapigenerator/testNoSchemas/assets/IOnTest.java",
      "asyncapigenerator/testNoSchemas/assets/IOnTest2.java",
      "asyncapigenerator/testNoSchemas/assets/Producer.java"
    );

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateNestedObject () {
    final String API_FOLDER = "generated/com/sngular/scsplugin/nestedobject";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/nestedobject/model";

    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/testNestedObjectIssue/assets/payload/SomeOtherObject.java",
      "asyncapigenerator/testNestedObjectIssue/assets/payload/UserSignedUpPayload.java"
    );

    final List<String> expectedProducerFiles = List.of();

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateNotGeneratedPropertiesIssue () {
    final String API_FOLDER = "generated/com/sngular/scsplugin/notgeneratedproperties";

    final String MODEL_SCHEMA_FOLDER = "generated/com/sngular/scsplugin/notgeneratedproperties/model";


    final List<String> expectedModelSchemaFiles = List.of(
      "asyncapigenerator/testPropertiesNotGeneratedIssue/assets/payload/UserDetails.java",
      "asyncapigenerator/testPropertiesNotGeneratedIssue/assets/payload/UserSignedUp.java"
    );

    final List<String> expectedProducerFiles = List.of();

    return path -> modelTest(path, expectedModelSchemaFiles, MODEL_SCHEMA_FOLDER) &&
                   modelTest(path, expectedProducerFiles, API_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithKafkaBindings() {

    final String DEFAULT_COMMON_FOLDER = "generated/com/sngular/scsplugin/filegenerationwithkafkabindings/model/event";

    final String DEFAULT_CONSUMER_FOLDER = DEFAULT_COMMON_FOLDER + "/consumer";

    final String DEFAULT_PRODUCER_FOLDER = DEFAULT_COMMON_FOLDER + "/producer";

    final String DEFAULT_MODEL_SCHEMA_FOLDER = DEFAULT_COMMON_FOLDER;

    final String DEFAULT_CUSTOM_VALIDATOR_FOLDER = DEFAULT_COMMON_FOLDER + "/customvalidator";

    final String COMMON_PATH = "asyncapigenerator/testFileGenerationWithKafkaBindings/";

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

    final List<String> expectedModelMessageFiles = List.of(
        ASSETS_PATH + "OrderCreatedDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER, emptyList(), null) &&
                     modelTest(path, expectedModelSchemaFiles, DEFAULT_MODEL_SCHEMA_FOLDER) &&
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
      assertThat(targetConsumerFolder).isNotEmptyDirectory();
      TestUtils.validateFiles(expectedFile, targetConsumerFolder);

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetProducer = pathToTarget.resolve(targetProducer);
        final File targetProducerFolder = pathToTargetProducer.toFile();
        assertThat(targetProducerFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetProducerFolder);
      }

      if (CollectionUtils.isNotEmpty(expectedExceptionFiles)) {
        Path pathToTargetException = pathToTarget.resolve(targetException);
        File targetModelException = pathToTargetException.toFile();
        assertThat(targetModelException).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedExceptionFiles, targetModelException);
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

  private static String calculateJavaEEPackage(int springBootVersion) {
    if (3 <= springBootVersion) {
      return "jakarta/";
    } else {
      return "javax/";
    }
  }
}
