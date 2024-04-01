# SCS MultiApi Plugin

[![Codacy Badge](https://app.codacy.com/project/badge/Grade/4a9be5a4b6ab48afba293b2315edd47e)](https://app.codacy.com/gh/sngular/scs-multiapi-plugin/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)[![Maven Central](https://img.shields.io/maven-central/v/com.sngular/scs-multiapi-maven-plugin.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.sngular%22%20AND%20a:%22scs-multiapi-maven-plugin%22)

This is a plugin designed to help developers automatizing the creation of
code classes from YML files based on AsyncApi and OpenAPI. It is presented in 2 flavours
Maven and Gradle

## Index

- [SCS MultiApi Plugin](#scs-multiapi-plugin)
- [Index](#index)
- [Main Configuration](#main-configuration)
  - [How to configure the POM file](#how-to-configure-the-pom-file)
  - [How to configure the build.gradle file](#how-to-configure-the-build-file)
- [AsyncApi Generator](#asyncapi-generator)
  - [Configuration](#configuration)
    - [Generated Sources Folder](#generated-sources-folder)
  - [How is apiPackage set?](#how-is-apipackage-set)
  - [How is modelPackage set?](#how-is-modelpackage-set)
  - [Class Generation](#class-generation)
    - [Consumer and Supplier classes](#consumer-and-supplier-classes)
      - [Method interfaces](#method-interfaces)
      - [Mapper](#mapper)
        - [Implementation](#implementation)
      - [Stream Bridge class](#stream-bridge-class)
- [OpenApi Generator](#openapi-generator)
  - [Getting Started](#getting-started)
  - [Initial Considerations](#initial-considerations)
  - [Usage](#usage)
- [Property Validation](#property-validation)

## Main Configuration

This plugin allows developers to automatize the creation of code classes for
REST and Kafka connections, based on YML files under the AsyncApi and OpenApi
specifications. In the latter case, many of the configuration options and classes
that are generated are based on reimplementation or modification of the OpenAPI Generator
models and template designs.

The generation of the REST and Kafka connections is independent each other and
could be used only one, or both at the same time.

Here is the documentation for these technologies:

- [OpenApi](https://swagger.io/specification/)
- [AsyncApi](https://www.asyncapi.com/docs/getting-started)
- [OpenAPI Generator](https://openapi-generator.tech/docs/configuration)

### How to configure the POM file

To maintain the generation of the different types of classes independent, they
are configured as two different goals on the plugin, `asyncapi-generation` and
`openapi-generation`.
As commented above, they both could be used at the same time, setting a double
*execution* for the plugin in the `pom.xml` file.

```xml

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>5.4.1</version>
  <executions>
    <execution>
      <id>asyncapi</id>
      <phase>generate-sources</phase>
      <goals>
        <goal>asyncapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          ...
        </specFiles>
      </configuration>
    </execution>

    <execution>
      <id>openapi</id>
      <phase>generate-sources</phase>
      <goals>
        <goal>openapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            ...
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
</plugin>
```

In the example above, you can see a partial configuration for the plugin with
a double *execution*. This makes necessary to set an `id` for each execution,
`asyncapi` and `openapi` in this case.

In the case that you only want to run one of the goals of the plugin, you only
need to remove the *execution* section that you don't need.

In the [AsyncApi Generator](#asyncapi-generator) and the
[OpenApi Generator](#openapi-generator) sections, you can find more information
about how they work, and the parameters and configuration options they offer.

### How to configure the build file

To maintain the generation of the different types of classes independent, they
are configured as two different task on the plugin, `openApiTask` and
`asyncApiTask`.
Apply the plugin in the `build.gradle` file and invoke the task.

```groovy
plugins {
  id "java"
  id "com.sngular.scs-multiapi-gradle-plugin' version '5.4.1"

  openapimodel {

  }

  asyncapimodel {

  }
}
```

In the example above, you can see a partial configuration for the plugin with
the extension configuration. Just create the (openapi|asyncapi)model objets to
configure the tasks.

In the case that you only want to run one of the goals of the plugin, you only
need to remove the *execution* section that you don't need.

In case no configuration is provided but only the file to generate an exception will be raised and an error will occur.
In the [AsyncApi Generator](#asyncapi-generator) and the
[OpenApi Generator](#openapi-generator) sections, you can find more information
about how they work, and the parameters and configuration options they offer.

## AsyncApi Generator

### Configuration

#### Maven

The plugin defined `phase` and `goal` parameters are expected to be
*generate-sources* and *asyncapi-generation*, as they are the only values for
which the plugin is designed.

```xml

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>5.4.1</version>
  <executions>
    <execution>
      <phase>generate-sources</phase>
      <goals>
        <goal>asyncapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>PATH_TO_YML</filePath>
          </specFile>
          <specFile>
            <filePath>PATH_TO_YML</filePath>
            <consumer>
              <ids>publishOperation</ids>
              <classNamePostfix>MY_CONSUMER_CLASS</classNamePostfix>
              <modelNameSuffix>DTO</modelNameSuffix>
              <apiPackage>com.sngular.apigenerator.asyncapi.business_model.model.event.consumer</apiPackage>
              <modelPackage>com.sngular.apigenerator.asyncapi.business_model.model.event</modelPackage>
            </consumer>
            <supplier>
              <ids>subscribeOperation</ids>
              <apiPackage>com.sngular.apigenerator.asyncapi.business_model.model.event.producer</apiPackage>
              <modelPackage>com.sngular.apigenerator.asyncapi.business_model.model.event</modelPackage>
            </supplier>
          </specFile>
        </specFiles>
        <generatedSourcesFolder>sources-generated</generatedSourcesFolder>
      </configuration>
    </execution>
  </executions>
</plugin>
```

#### Gradle

In this case we have an extension model to fulfill. Similar to the Maven one.

```groovy
openapimodel {
  specFile {
    {
      filePath = './src/main/resources/api/rest/api-rest.yml'
      apiPackage = 'com.sngular.world_domination.api'
      modelPackage = 'com.sngular.world_domination.model'
      useTagsGroup = true
    }
    overWriteModel = true
  }
}
```

As you can see in the example above, there is a main parameter **specFiles**
that receives a list of **specFile** attributes groups, so you can set as many
YML files as you want.

**specFiles** could be configured in two different ways:

1. The first one is to configure only the YML file. This is made using the
   **filePath** parameter, that expects to receive the path to the file. Using
   the plugin in this way, you can't configure the model package or the api
   package in the pom file, neither other options, so they will be configured as
   its explained in [apiPackage](#how-is-apipackage-set) and
   [modelPackage](#how-is-modelpackage-set) sections.  
   This way it's limited to the usage of Consumer and Supplier methods.

    ```xml
    <specFile>
        <filePath>PATH_TO_YML</filePath>
    </specFile>
    ```

2. The second one is to configure the YML file with the consumers, supplier
   producers and streamBrige producers that you want to generate.

  ```xml

<specFile>
  <filePath>PATH_TO_YML</filePath>
  <consumer>
    <ids>publishOperation</ids>
    <classNamePostfix>MY_CONSUMER_CLASS</classNamePostfix>
    <modelNameSuffix>DTO</modelNameSuffix>
    <apiPackage>com.sngular.apigenerator.asyncapi.business_model.model.event.consumer</apiPackage>
    <modelPackage>com.sngular.apigenerator.asyncapi.business_model.model.event</modelPackage>
  </consumer>
  <supplier>
    <ids>subscribeOperation</ids>
    <apiPackage>com.sngular.apigenerator.asyncapi.business_model.model.event.producer</apiPackage>
    <modelPackage>com.sngular.apigenerator.asyncapi.business_model.model.event</modelPackage>
  </supplier>
  <streamBridge>
    <ids>streamBridgeOperation</ids>
    <apiPackage>com.sngular.apigenerator.asyncapi.business_model.model.event.producer</apiPackage>
    <modelPackage>com.sngular.apigenerator.asyncapi.business_model.model.event</modelPackage>
  </streamBridge>
</specFile>
  ```

  ```groovy
  specFile {
  {
    filePath = './src/main/resources/api/event/event-api.yml'
    consumer {
      ids = 'publishOperation'
      apiPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event.consumer'
      modelPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event'
    }
    supplier {
      ids = 'subscribeOperation'
      apiPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event.producer'
      modelPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event'
    }
    streamBridge {
      ids = 'streamBridgeOperation'
      apiPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event.producer'
      modelPackage = 'com.sngular.apigenerator.asyncapi.business_model.model.event'
    }
  }
  overWriteModel = true
}
  ```

As you can see in the example above, there are three blocks of parameters that
can be configured in the plugin.

- **filePath**: This parameter works in the same way as in the first option.
- **consumer**, **supplier** and **streamBridge**: They are both configured in
  the same way and can receive the same parameters. These parameters are:
  - **ids**: With this parameter you can set the operationId that you want to
      be generated as subscriber or publisher. If this parameter is not defined for
      the `consumer` section, all the subscribe operations defined in the YML file,
      will be generated. If only one of `supplier` and `streamBridge` sections are
      defined, and this parameter is not defined inside it, all the publish
      operations defined in the YML file will be generated. If both `supplier` and
      `streamBridge` sections are defined, it`s needed to define which operations
      belong to each category.
  - **classNamePostfix**: This parameter receives the name of the class that
      it's going to be generated containing the Beans. This parameter is optional,
      and by default the classes will be called `Producer`, `StreamBridgeProducer`
      and `Subscriber`.
  - **modelNameSuffix**: With this parameter you can set the suffix that is
      going to be used in the entities of the generated classes. For example if
      you set this to `DTO`, and there is a class named `EntityClass`, it will
      result as `EntityClassDTO`. This parameter is optional.
  - **apiPackage**: This parameter receive a package name, where the
      generated classes will be generated. This parameter is optional.
      Check [how is the apiPackage set](#how-is-apipackage-set) for
      more information about how this parameter works, and the values it
      could have.
  - **modelPackage**: This parameter receives a package name, where the entities
      used for the generated classes are defined. As it's explained in the
      [Mapper Section](#mapper), those entities are usually auto-generated, so the
      plugin expects the modelPackage to be the package where them are included.
      **Note that the plugin doesn't create the entities neither checks their
      existence**, it takes their names from the YML file and assume that they are
      created by the user. As the previous parameter, this is also optional.
      Check [how is the modelPackage set](#how-is-modelpackage-set) for more
      information about how his parameter works, and the values it could have.
  -  **dateFormat**: This parameter changes the format annotation for `LocalDate` fields.
      The syntax follow the [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html).
      The default value are `yyyy-MM-dd`.
  -  **dateTimeFormat**: This parameter changes the format annotation for `LocalDateTime`
      fields. The syntax follow the [Java SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html).
      The default value are `yyyy-MM-dd'T'HH:mm:ss`.
  -  **useTimeType**: Enum TimeType value. Controls the types used when generating dates. Can be `LOCAL` or `ZOINED`.
      The default value is `TimeType.LOCAL`. 

The configuration of `consumer`, `supplier` and `streamBridge` are independent.
If only one of them is configured in the pom file, only that one will be
generated.

#### Generated Sources Folder

There is also an independent parameter that affects to all the *specFiles*
generated, which is called **generatedSourcesFolder**. This parameter expects
to receive a string, that could include letters, numbers and `-`, with the
name of the folder where generated sources by the plugin will be located.

By default, it's values is `generated-sources`, so the files will be in
`.../target/generated-sources/apigenerator/...`. If you set another value in
the pom.xml file, as in the example above, files will remain in
`.../target/sources-generated/apigenerator/...`.

### How is apiPackage set?

The api package could be set in three different ways.

- **User definition**: The user provides a package name using the parameter in
  the pom.xml file.
- **GroupID from YML**: If the user doesn't provide a package name, the plugin
  will try to use the `groupId` attribute from the YML file that is in use.
- **Default package name**: If neither of the previous options were given, the
  plugin will use a default package name, that is stablished as
  `com.sngular.apigenerator.asyncapi`.

### How is modelPackage set?

The model package could be set in four different ways.

- **User definition**: The user provides a package name using the parameter in
  the pom.xml file.
- **Namespace from YML**: If the user doesn't provide a package name, the
  plugin will check if the entity name definition in the YML file, includes a
  complete package name.

```yaml
order/createCommand:
  subscribe:
    operationId: "subscribeOperation"
    message:
      $ref: '#/components/messages/com.sngular.apigenerator.asyncapi.model.CreateOrder'
```

- **Namespace from Avro**: The plugin will check for a `namespace`
  attribute defined in the Avro file and use it, if a namespace is
  not defined it will throw an exception. The plugin expects to receive
  a relative path from the `yml` file folder.

```yaml
order/created:
  publish:
    operationId: "publishOperation"
    message:
      $ref: 'path_to_Avro_file'
```

- **Default package name**: If neither of the previous options were given, the
  plugin will use a default package name, that is stablished as
  `com.sngular.apigenerator.asyncapi.model`.

### Class Generation

#### Consumer and Supplier classes

Those are a pair of classes, separated by the directionality of the messages.
They came from the plugin fully implemented by making reference to the
interfaces of the next section. Their names could be modified using the
`classNamePostfix` parameter specified on the
[Usage section](#usage), being by default **Producer** and
**Subscriber**.

```java

@Configuration
public class StreamTopicListenerConsumer {

  private final ISubscribeOperation subscribeOperation;

  protected StreamTopicListenerConsumer(final ISubscribeOperation subscribeOperation) {
    this.subscribeOperation = subscribeOperation;
  }

  @Bean
  public Consumer<CreateOrder> consumerSubscribeOperation() {
    return value -> subscribeOperation.subscribeOperation(value);
  }
}
```

This sample class, is related to the previously used YML file, and in it, you
could see that it came fully implemented, based on the related Interface that
lets the personalization and implementation to the user. Also, in this example
is possible to see how the YML attribute 'operationId' is used to name the
methods as `Consumer'OperationId'` or `Publisher'OperationId'`.

##### Method interfaces

Those are a group of interfaces that are related to the previous seen classes.
There are as many as operations are defined in the YML file, and in the
previous classes, so there is only one operation defined in each interface.

This layer is the only one that needs work by the end user, so it needs to
implement these interfaces.

These interfaces are named following the "I*OperationId*" pattern, where
'OperationId' comes from the YML file definition of the channels section.
The method is named as 'OperationId' as well as on the classes in the
above section.

```java
public interface ISubscribeOperation {

  void subscribeOperation(CreateOrder value);
}
```

#### Bindings

Asyncapi support a way to specify specific configuration for certain protocols. Nowadays we only support Kafka specific information to define a Key form Messages as you can find [here](<https://github.com/asyncapi/bindings/blob/master/kafka/README.md>).
When a binding is specified in a message we will generate a generic class named as MessageWrapper which will contain the payload and the key used in to build a Message.
You will find such class by each api package you define.

##### Mapper

The entities used for the definitions both on the previous seen classes and
this interfaces, are auto-generated entities, based on the same YML file.
Because of that, they need to be mapped to a user defined entity using a mapper
utility class.

This mapper must be defined by the user on its own way to improve the
personalization capabilities of the plugin.

Down here you have an example of the mapper utility class as well as a simple
class implementing the interface defined above.

```java

@Mapper
public interface Mapper {

  Order map(com.sngular.apigenerator.asyncapi.business_model.model.event.Order value);
}
```

###### Implementation

```java

@Component
public class SubscribeOperation implements ISubscribeOperation {

  private final Mapper mapper;

  public subscribeOperation(final Mapper mapper) {this.mapper = mapper;}

  @Override
  public void subscribeOperation(final Order value) {
    com.sngular.apigenerator.asyncapi.business_model.model.Order orderMapped = mapper.map(value);
    //TODO: implement the functionality
  }
}
```

#### Stream Bridge class

In this case, there is only one class where all the selected operations will be
included. It's name could be modified using the `classNamePostfix` parameter
specified on the [Usage section](#usage), being by default
**StreamBridgeProducer**.

```java

@Configuration
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public void streamBridgeOperation(CreateOrder createOrder) {
    streamBridge.send("publishOperation", createOrder);
  }
}
```

This sample class, is related to the previosly used YML file, and in it you
could see that it came fully implemented.

Also, it's important to note that using Stream Bridge, the *binding* where the
messages are going to be sent is included in the auto generated class. This is
defined by the application properties using `function`, `binders` and
`bindings`, as in the next example:

```yaml
spring:
  kafka:
    bootstrap-servers: localhost:xxxx
    producer:
      client-id: peter
      key-serializer: org.apache.kafka.common.serialization.StringSerializer
      value-serializer: org.springframework.kafka.support.serializer.JsonSerializer
  cloud:
    function:
      definition: publishOperation
    stream:
      defaultBinder: kafka
      bindings:
        publishOperation:
          destination: orderCreated
      binders:
        kafka:
          defaultCandidate: true
          type: kafka
          producer-properties:
            key.serializer: org.apache.kafka.common.serialization.StringSerializer
            value.serializer: org.springframework.kafka.support.serializer.JsonSerializer
```

Because the plugin cannot access the application properties, the name of the
corresponding *binding* must be used as the **channel identifier** in the YML
file that's set on the plugin configuration, as you can see on the next
extract:

```yaml
channels:
  publishOperation:
    subscribe:
      operationId: "streamBridgeOperation"
      message:
        $ref: '#/components/messages/CreateOrder'
```

Due to the limitations on topics naming, the identifier of the channels that
are going to be used as Stream Bridge publishers, **only could include `-` or
`.` as separators**, slash `/` is not allowed.

## OpenApi Generator

### Getting Started

In order to get this plugin working, you need the following things installed
in your computer:

- Java 11 Version
- Maven

Depending on the approach with which you are going to use the plugin, other
dependencies will be necessary, for example:

- spring-boot-starter-webflux, in case you want to implement an API with
  responses in Mono/Flux Reactor types or use them for external calls through
  Spring WebClient.

After you have these installed, you need to add this plugin in your pom.xml or build.gradle
file. Here is an example of a basic configuration:

```xml

<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>5.4.1</version>
  <executions>
    <execution>
      <goals>
        <goal>openapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>${project.basedir}/src/main/resources/api/api.yml</filePath>
            <apiPackage>com.sngular.apigenerator.openapi.api</apiPackage>
            <modelPackage>com.sngular.apigenerator.openapi.api.model</modelPackage>
            <modelNameSuffix>DTO</modelNameSuffix>
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
</plugin>
```

```groovy
openapimodel {
  specFile {
    {
      filePath = './src/main/resources/api/api.yml'
      apiPackage = 'com.sngular.apigenerator.openapi.api'
      modelPackage = 'com.sngular.apigenerator.openapi.api.model'
      useTagsGroup = true
    }
    overWriteModel = true
  }
}
```

### Initial Considerations

Before using this plugin we have to warn that not all the complexity and
support offered by the use of swagger.io yml files is supported.

Since 1.1.0 version, we support the definition of parameters in both Path
and Operation object. ❗❗❗ Please bear in mind that we use the Option
resolver from OpenApi which will override the Operation parameters
if you have a parameter defined in the Path.

We establish here some of these options that are not yet supported and that
will be added to this plugin as time goes by and the existing need among users.

- Using Multiple Authentication Types within the security options both at an
  operational and general level.

- The use of OAuth 2 and OpenID Connect Discovery Authentication Types.

### Usage

This plugin allows us to create multiple apis with just one maven clean
install execution, in this way the user can configure several specFiles tags
with different uses, thus generating Apis in the two possible modes: send or
receive calls, depending on the options of configuration selected in said
specFiles.

```xml

<configuration>
  <specFiles>
    <specFile>
      <filePath>${project.basedir}/src/main/resources/api/api.yml</filePath>
      <apiPackage>com.sngular.apigenerator.openapi.api</apiPackage>
      <modelPackage>com.sngular.apigenerator.openapi.api.model</modelPackage>
      <modelNameSuffix>DTO</modelNameSuffix>
    </specFile>
  </specFiles>
</configuration>
```

```groovy
openapimodel {
  specFile {
    {
      filePath = './src/main/resources/api/api.yml'
      apiPackage = 'com.sngular.apigenerator.openapi.api'
      modelPackage = 'com.sngular.apigenerator.openapi.api.model'
      useTagsGroup = true
    }
    overWriteModel = true
  }
}
```

To customize these specFiles tags we are going to specify them inside the
configuration tag, we must declare the specFiles tag that contains all files
that will be used. Each specFile has their own configuration:

| Name                     | Description                                                                                                                                                                                         | Example                                           |
|--------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| filePath                 | Path where the yaml is located                                                                                                                                                                      | ${project.basedir}/src/main/resources/api/api.yml |
| apiPackage               | Path where the api interface will be located                                                                                                                                                        | com.sngular.apigenerator.openapi                  |
| modelPackage             | Path where the models will be located                                                                                                                                                               | com.sngular.apigenerator.openapi.model            |
| modelNamePrefix          | Prefix that will be used ahead of every model´s name                                                                                                                                                | Api                                               |
| modelNameSuffix          | Suffix that will be used after every model´s name                                                                                                                                                   | DTO                                               |
| callMode                 | Boolean value to decide if you want to generate the api for external calls. **Use RestClient by default. It´s initialized to false by default**                                                     | false                                             |
| useTagsGroup             | Boolean value to decide if using tags instead of an URL for group the API. **It´s initialized to false by default**                                                                                 | false                                             |
| useLombokModelAnnotation | Boolean value to decide if you want your models with Lombok or not   **It´s initialized to false by default**                                                                                       | false                                             |
| isReactive               | Boolean value to decide if you want to generate the api with responses in Mono/Flux Reactor types. If callmode = true use WebClient instead of RestClient. **It´s initialized to false by default** | false                                             |
| useTimeType              | Enum TimeType value. Controls the types used when generating dates. Can be local, zoned, or offset. **Initialized to TimeType.LOCAL by default**                                                    | TimeType.OFFSET                                   |

As the configuration options already indicate, the data model will also be
created within the specified path.This model will be created with the indicated
prefixes and suffixes and the instances and imports will be made to that model
within the corresponding Api.

There are two properties configured outside the specFiles, the path where the
RestClient and the WebClient will be located, if this option is set in any
of the specFiles, and the name of the folder where the generated sources will
be saved in the api of the project.

| Name                                                | Description                                                                                                                                                                                                                                                                     | Example                                 |
|-----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------|
| clientPackage                                       | Path where the RestClient and/or WebClient are located                                                                                                                                                                                                                          | com.sngular.apigenerator.openapi.client |
| [generatedSourcesFolder](#generated-sources-folder) | Name of the folder, inside `target`, where the files will be located. By defaut it's `generated-sources`                                                                                                                                                                        | generated-sources                       |
| overwriteModel                                      | Boolean value to decide if you want your models to be overwritten if two or more models have the same name. True means that models will be overwritten and if false is set, it will throw an exception if two models share the same name. It is initialized to false by default | false                                   |
| springBootVersion                                   | The version of spring to target during generation. It's default value is `2`.                                                                                                                                                                                                   | 3                                       |

We must clarify that the options to make calls are configured under the
RestClient or WebClient specifications as indicated above in the configuration
options. If several of the APIs to be generated are defined under the same call
option, a single RestClient/Webclient will be generated for all of them, which
is initialized with the specific options needed within the class that defines
each API.

### Usage considerations

This plugin has been implemented trying to behave like OpenApi Generator Tool,
but we decided to change the approach concerning the support of AllOfs, OneOfs
and AnyOfs.

Every property that has been indicated in any of these types will be generated
in the model entity.

The way the model will behave changes depending on whether it is an AllOf, or
an AnyOf/OneOf:

If it is an AllOf, every property referenced will be treated as required
regardless of which ones are defined in the "required" field of the allOf
structure.

If it is an AnyOf or an OneOf, the plugin will only mark as required the
properties that have been defined as such in the "required" field of these
structures. After that, the constructor will check that at least one of the
properties will have a value, nothing else, so it is up to the user to fulfill
the restrictions he needs for the entity.

**IMPORTANT NOTE**: As previously stated, OneOf and AnyOf will behave the same,
this means that OneOf will work the same way as an AnyOf.

## Property validation

Both AsyncAPI and OpenAPI offer the possibility to add properties and apply constraints
to the values a certain object can take. To validate these properties,
we annotate the pertaining fields and generate the corresponding validators so that the user
can later use a framework such as Hibernate to check for correctness.

## Loading specifications from dependencies

The plugin supports loading API specification YMLs from the classpath. This will be the
location it searches for them in first, falling back to the project directories otherwise.

Here's an example configuration for the maven plugin that loads an AsyncAPI specification
from a local JAR containing `contracts/event-api.yml` in its resources:

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <executions>
    <execution>
      <id>asyncapi</id>
      <phase>generate-sources</phase>
      <goals>
        <goal>asyncapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>contracts/event-api.yml</filePath>
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
  <dependencies>
    <dependency>
      <groupId>com.sngular</groupId>
      <artifactId>yml-source</artifactId>
      <version>1.0</version>
      <scope>system</scope>
      <systemPath>${project.basedir}/yml-source-1.0.jar</systemPath>
    </dependency>
  </dependencies>
</plugin>
```
