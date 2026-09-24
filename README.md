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
  - [Calling an API from your service (callMode)](#calling-an-api-from-your-service-callmode)
  - [Object-typed query parameters and multipart bodies](#object-typed-query-parameters-and-multipart-bodies)
  - [Camel case Java names (useCamelCaseNames)](#camel-case-java-names-usecamelcasenames)
  - [Unknown enum values (useUnknownEnumValue)](#unknown-enum-values-useunknownenumvalue)
- [Property Validation](#property-validation)
- [Loading specifications from the plugin classpath](#loading-specifications-from-the-plugin-classpath)
- [Loading specifications from a remote URL](#loading-specifications-from-a-remote-url-apicurio-registry-http)
- [Loading Specs from a Published Artifact](#loading-specs-from-a-published-artifact)
- [Architecture](docs/ARCHITECTURE.md)
- [Loading specs](docs/LOADING_SPECS.md)

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
  <version>5.4.3</version>
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

#### Mandatory dependencies

These dependencies are used by generated code

```xml
 <dependencies>
       <dependency>
            <groupId>io.swagger.parser.v3</groupId>
            <artifactId>swagger-parser-core</artifactId>
            <version>2.1.20</version>
        </dependency>
        
        <dependency>
            <groupId>io.swagger.core.v3</groupId>
            <artifactId>swagger-annotations-jakarta</artifactId>
            <version>2.2.20</version>
        </dependency>
        
        <dependency>
            <groupId>jakarta.validation</groupId>
            <artifactId>jakarta.validation-api</artifactId>
            <version>3.0.2</version>
        </dependency>
 </dependencies>
```

### How to configure the build file

To maintain the generation of the different types of classes independent, they
are configured as two different task on the plugin, `openApiTask` and
`asyncApiTask`.
Apply the plugin in the `build.gradle` file and invoke the task.

```groovy
plugins {
  id "java"
  id "com.sngular.scs-multiapi-gradle-plugin' version '5.4.3"

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

#### Mandatory gradle dependencies

These dependencies are used by generated code

``` gradle
implementation 'io.swagger.parser.v3:swagger-parser-core:2.1.20'
implementation 'io.swagger.core.v3:swagger-annotations-jakarta:2.2.20'
implementation 'jakarta.validation:jakarta.validation-api:3.0.2'
```

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
  <version>5.4.3</version>
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
      be generated as subscriber or publisher. It can be a single
      [`String`]: for example `ids = 'publishOperation'`, or a list of them:
      for example `ids = ['publishOperation', 'anotherOperation']`. If a list is
      provided, each element of the list will be generated. If this parameter is
      not defined for the `consumer` section, all the subscribe operations defined
      in the YML file, will be generated. If only one of `supplier` and
      `streamBridge` sections are defined, and this parameter is not defined inside
      it, all the publish operations defined in the YML file will be generated. If
      both `supplier` and `streamBridge` sections are defined, it`s needed to define
      which operations belong to each category.
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
  -  **generateSpringwolfAnnotations**: Boolean value. When set to `true`, the generated
      `Subscriber` and `Producer` bean methods are annotated with Springwolf's
      `@AsyncListener` / `@AsyncPublisher` (with the channel name and `operationId`).
      **It's initialized to `false` by default**. Only applies to `consumer` and `supplier`
      sections (not `streamBridge`), and not in combination with Kafka bindings.
  -  **useUnknownEnumValue**: Boolean value. When `true`, every generated enum reads
      a value outside the contract as `UNKNOWN` instead of failing, as described in
      [Unknown enum values](#unknown-enum-values-useunknownenumvalue).
      **It's initialized to `true` by default** for AsyncAPI. Set it to `false` for strict enums.

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

Asyncapi support a way to specify specific configuration for certain protocols.

Nowadays, we only support Kafka specific information to define a Key form.

Messages as you can find
[here](<https://github.com/asyncapi/bindings/blob/master/kafka/README.md>).

When a binding is specified in a message we will generate a generic class
named as MessageWrapper which will contain the payload and the key
used in to build a Message.
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
  <version>5.4.3</version>
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
      consumer {
        apiPackage = 'com.sngular.apigenerator.openapi.api'
        modelPackage = 'com.sngular.apigenerator.openapi.api.model'
      }
      supplier {
        apiPackage = 'com.sngular.apigenerator.openapi.api'
        modelPackage = 'com.sngular.apigenerator.openapi.api.model'
      }
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
| clientComponent          | With `callMode`, whether the `*Api` client classes are `@Component`s. `false`: declare them yourself ([see](#calling-an-api-from-your-service-callmode)). **It´s initialized to true by default**   | false                                             |
| useHttpExchange          | With `callMode`, generates `@HttpExchange` interfaces; needs `springBootVersion` >= 3 ([see](#calling-an-api-from-your-service-callmode)). **It´s initialized to false by default**                 | true                                              |
| useCamelCaseNames | Camel-case Java names, JSON as-is. Default false | true |
| useUnknownEnumValue | Enums read values outside the contract as `UNKNOWN` instead of failing ([see](#unknown-enum-values-useunknownenumvalue)). **Unset, it's `true` with `callMode` (clients) and `false` otherwise (servers)** | true |

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
| springBootVersion                                   | Spring Boot version to target, `MAJOR` or `MAJOR.MINOR` (`3` means 3.0). Default `2`. `>= 3`: `jakarta.*` instead of `javax.*`; `>= 3.2`: adds `ApiRestClient(RestClient)`; `>= 4`: Jackson 3 (`tools.jackson.*`) and Spring 7 imports (see below).                             | 3.2                                     |

We must clarify that the options to make calls are configured under the
RestClient or WebClient specifications as indicated above in the configuration
options. If several of the APIs to be generated are defined under the same call
option, a single RestClient/Webclient will be generated for all of them, which
is initialized with the specific options needed within the class that defines
each API.

### Spring Boot 4 / Jackson 3 support

Setting `springBootVersion` to `4` (or higher) targets Spring Boot 4
(Spring Framework 7), which ships **Jackson 3** as its default JSON stack.
When this is set, the generated code changes as follows:

- Jackson `databind` imports are emitted under `tools.jackson.*` instead of
  `com.fasterxml.jackson.*` (e.g. `tools.jackson.databind.annotation.JsonDeserialize`).
- Jackson **annotations** keep their original coordinates
  (`com.fasterxml.jackson.annotation.*`, such as `@JsonProperty`), matching
  Jackson 3's own packaging.
- Generated RestClient/WebClient build an immutable `JsonMapper` via
  `JsonMapper.builder()` (the Jackson 2 mutable `ObjectMapper` API is gone), and
  the reactive WebClient uses the Spring Framework 7 codecs
  (`JacksonJsonEncoder`/`JacksonJsonDecoder`).

For `springBootVersion < 4` the output is unchanged and keeps targeting Jackson 2.

> **Note:** Lombok-annotated models (`useLombokModelAnnotation`) rely on Lombok's
> `@Jacksonized`, which does not yet support Jackson 3. Combining Lombok models
> with `springBootVersion = 4` is therefore not supported.

### Calling an API from your service (callMode)

With `callMode = true` the plugin generates a client for the API instead of the
interfaces a server implements. A service that calls another one almost always
needs to configure that client itself: the base URL changes per environment,
and authentication, timeouts or message converters come from the service's own
configuration, not from the contract. The generated client supports two ways of
doing that, and both leave the choice to you:

- [Generated client class](#generated-client-class-default), the default:
  a class that calls through `ApiRestClient` (`RestTemplate`) or
  `ApiWebClient` (`WebClient`). Works on Spring Boot 2, 3 and 4.
- [HTTP service interface](#http-service-interface-usehttpexchange)
  (`useHttpExchange`): an `@HttpExchange` interface that Spring implements over
  your own `RestClient` or `WebClient`. Works on Spring Boot 3 and 4.

Nothing changes unless you configure it: a project that already uses `callMode`
gets the same generated behaviour as before.

> **Only need the models?** If you call the API with code of your own and only
> use the generated models, generate with `callMode = false`. The models are
> identical, and the `*Api` interfaces generated alongside them register no
> beans and make no calls unless a class implements them.

#### Generated client class (default)

Each `*Api` is a class that sends its requests through `ApiRestClient`
(`RestTemplate`), or `ApiWebClient` (`WebClient`) when `isReactive = true`. It
works with every supported Spring Boot version. Out of the box it builds its own
HTTP client and sends requests to the **first `servers` URL** of the contract,
and the imperative `*Api` classes are `@Component`s.

To use the service's own configuration instead:

1. **Give it your configured client.** `ApiRestClient(RestTemplate)` and
   `ApiWebClient(WebClient)` use the client you pass instead of building one, so
   its interceptors, timeouts and converters apply. With `springBootVersion`
   `3.2` or later (`RestClient` is part of Spring Framework 6.1), there is also
   `ApiRestClient(RestClient)`, whose `baseUrl` applies to an empty base path.
   There are also variants that take the `Map<String, Authentication>` for the
   contract's security schemes, when you want the generated authentication
   classes applied too.
2. **Choose the base URL.** Every `*Api` has a `basePath`: the first `servers`
   URL by default, or the one you pass to its constructor or `setBasePath`.
   An **empty** base path sends requests relative to the client's own root,
   as set with `RestTemplateBuilder.rootUri(...)` (or a
   `DefaultUriBuilderFactory` base URL) or `WebClient.Builder.baseUrl(...)`.
   That keeps the environment's URL in your configuration, whatever order the
   contract lists its `servers` in.
3. **Declare the beans yourself.** With `clientComponent = false` the imperative
   `*Api` classes are no longer `@Component`s, so you register them with the
   client you configured. (The reactive `*Api` classes never were components.)

```xml
<specFile>
  <filePath>openapi/openapi.yml</filePath>
  <apiPackage>com.acme.clients.api</apiPackage>
  <modelPackage>com.acme.clients.model</modelPackage>
  <clientPackage>com.acme.clients.client</clientPackage>
  <callMode>true</callMode>
  <clientComponent>false</clientComponent>
</specFile>
```

```java
@Configuration
class ClientsApiConfiguration {

  @Bean
  ClientsApi clientsApi(RestTemplateBuilder builder, ClientsProperties props) {
    RestTemplate restTemplate = builder
        .rootUri(props.baseUrl())   // per environment
        .setReadTimeout(Duration.ofSeconds(5))
        .additionalInterceptors(new CorrelationIdInterceptor())
        .build();
    // An empty base path sends requests relative to the root URI.
    return new ClientsApi(new ApiRestClient(restTemplate), "");
  }
}
```

The reactive equivalent passes
`new ApiWebClient(webClientBuilder.baseUrl(...).build())`. The same
constructors are available with `clientComponent` left at `true`; Spring then
keeps creating the component with its no-argument constructor, as before.

#### HTTP service interface (useHttpExchange)

On Spring Boot 3 and later you can generate each `*Api` as a
[Spring HTTP service interface](https://docs.spring.io/spring-framework/reference/integration/rest-clients.html#rest-http-interface)
instead. Spring implements it at runtime over the `RestClient` (or
`WebClient`, with `isReactive = true`) you configure, so the plugin generates no
HTTP code at all:

- operations are `@GetExchange`, `@PostExchange`, `@PutExchange`,
  `@PatchExchange` or `@DeleteExchange` methods, with paths **relative** to
  your client's base URL;
- parameters are bound as the contract declares them, with their names, their
  `default` as `defaultValue`, and `@DateTimeFormat` for `format: date` and
  `format: date-time`;
- JSON bodies are `@RequestBody`, and `multipart/form-data` bodies become one
  `@RequestPart` per part;
- each method returns `ResponseEntity<T>`, or `Mono<ResponseEntity<T>>` when
  reactive;
- no `ApiRestClient`/`ApiWebClient`, authentication classes or `servers` URL are
  generated. Authentication belongs to the client you configure, for example as
  an interceptor or default header.

`useHttpExchange` needs `callMode = true` and `springBootVersion` 3 or later
(`@HttpExchange` is part of Spring Framework 6). Generation fails with a clear
message otherwise.

```xml
<specFile>
  <filePath>openapi/openapi.yml</filePath>
  <apiPackage>com.acme.clients.api</apiPackage>
  <modelPackage>com.acme.clients.model</modelPackage>
  <callMode>true</callMode>
  <useHttpExchange>true</useHttpExchange>
</specFile>
```

Generated interface, for reference:

```java
@HttpExchange
public interface ClientsApi {

  @GetExchange(url = "/clients/{client_id}", accept = {"application/json"})
  ResponseEntity<ClientDTO> getClient(
      @PathVariable("client_id") Long client_id,
      @RequestParam(name = "since", required = false)
      @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate since,
      @RequestHeader(name = "X-Correlation-Id", required = false)
      String xCorrelationId);

  @GetExchange(url = "/clients", accept = {"application/json"})
  ResponseEntity<List<ClientDTO>> searchClients(
      @RequestParam(name = "page_num", required = false, defaultValue = "0")
      Integer page_num);
}
```

**Spring Boot 3 (and 4): with `HttpServiceProxyFactory`.** Build the interface
over your configured client:

```java
@Configuration
class ClientsApiConfiguration {

  @Bean
  ClientsApi clientsApi(RestClient.Builder builder, ClientsProperties props) {
    RestClient restClient = builder
        .baseUrl(props.baseUrl())
        .requestInterceptor(new CorrelationIdInterceptor())
        .build();
    return HttpServiceProxyFactory
        .builderFor(RestClientAdapter.create(restClient))
        .build()
        .createClient(ClientsApi.class);
  }
}
```

For the reactive variant use `WebClientAdapter.create(webClient)` instead.

**Spring Boot 4: with `@ImportHttpServices`.** Spring Boot 4 can register the
interfaces as beans and configure their clients from properties (it needs
`spring-boot-starter-restclient`, or `spring-boot-starter-webclient` for the
reactive variant):

```java
@Configuration
@ImportHttpServices(group = "clients", types = ClientsApi.class)
class ClientsApiConfiguration {

  @Bean
  RestClientHttpServiceGroupConfigurer clientsGroupConfigurer() {
    return groups -> groups.filterByName("clients")
        .forEachClient((group, builder) ->
            builder.requestInterceptor(new CorrelationIdInterceptor()));
  }
}
```

```yaml
spring:
  http:
    serviceclient:
      clients:
        base-url: https://clients.pre.acme.com
        default-header:
          X-Source: [billing-service]
```

In Gradle, both options go in the `specFile` block of `openapimodel`:

```groovy
openapimodel {
  specFile {
    filePath = 'openapi/openapi.yml'
    callMode = true
    clientComponent = false   // generated client class, declared by you
    // useHttpExchange = true // or: HTTP service interfaces (Spring Boot 3+)
  }
}
```

### Schemas and properties named after Java reserved words

A contract is free to use names that Java cannot: a schema called `Package`, a
property called `new` or `class`. The generator keeps the contract untouched and
adapts the Java side instead.

- A **property** whose name is a reserved word becomes an underscore-prefixed
  Java field (`new` -> `_new`), and so do the builder method and the local
  variables that reference it. The JSON name is **not** changed: the field and
  the builder setter both carry `@JsonProperty("new")`, so the payload keeps
  using the name the contract declares.
- The **singular adder** generated for a collection is sanitized after it is
  singularized, so `packages` yields `_package(...)`, not `package(...)`. When
  singular and plural collapse onto the same identifier the adder takes an extra
  underscore (`new` yields `_new(List<...>)` plus `__new(...)`).
- A **schema** whose name is a reserved word once uncapitalized (`Package`) is
  generated as-is; only the local variables derived from it are prefixed.
- A property named `class` would produce `getClass()`, which cannot override
  `Object.getClass()`. Its accessors are generated as `get_class()` /
  `set_class()` instead. No other accessor is renamed.

> **Note:** Lombok-annotated models (`useLombokModelAnnotation`) derive their
> accessors and builder from Lombok, so the accessor and builder adjustments
> above do not apply to them.

### Names that Java cannot spell

Contracts routinely name things with characters Java does not accept in an
identifier: a header called `Idempotency-Key`, a query parameter called
`sort-by`, a property called `client-ref` or `delivery.status`. Those names
reach the generated code as identifiers, where they do not compile.

The generator rebuilds such a name in lower camel case out of its alphanumeric
runs, and leaves the contract name where it belongs - on the wire:

- A **parameter** named `Idempotency-Key` is declared as `idempotencyKey`. The
  binding keeps the contract name: `@RequestHeader(name = "Idempotency-Key")` on
  the API interface, `headerParams.add("Idempotency-Key", ...)` on the client.
  The same applies to query (`@RequestParam(name = "sort-by", ...)`), path
  (`@PathVariable("shipment-id")`) and cookie (`@CookieValue(name = ...)`)
  parameters.
- A **property** named `client-ref` becomes the field `clientRef`, and keeps
  `@JsonProperty("client-ref")` on both the field and the builder setter, so it
  is read and written under the name the contract declares.
- A name that would start with a digit is prefixed with an underscore
  (`2fa-token` -> `_2faToken`).
- A name that is already a legal Java identifier is never rewritten, so
  `shipment_id` stays `shipment_id`.

Header and cookie parameters are now bound explicitly with `@RequestHeader` and
`@CookieValue`. Before, they were declared without a binding annotation, which
made Spring resolve them as request parameters.

### Object-typed query parameters and multipart bodies

A query parameter whose schema is an object travels as its properties, as
OpenAPI serializes it by default (`style: form`, `explode: true`):
`?page_number=1&page_size=20`, or `?filters[page_number]=1` with
`style: deepObject`.

- The server interfaces and the `@HttpExchange` interfaces declare one
  `@RequestParam` per property (`Integer page_number, Integer page_size`), so
  Spring binds and sends each one. A `style: form, explode: false` object, which
  travels as a single `filters=page_number,1,page_size,20` value, keeps its
  object type.
- The generated client classes keep the object argument and expand it as its
  `style`/`explode` declare.

A `multipart/form-data` request body with an inline schema has no model of its
own: every generated API takes one argument per part (`MultipartFile file,
String comment`). A multipart body that references a component schema keeps
taking that model.

### Camel case Java names (useCamelCaseNames)

By default the generated Java code uses the names the contract gives, whenever
they are legal Java identifiers: a parameter `page_num` is declared as
`page_num`, and a property `total_items` gives `getTotal_items()`. Only names
Java cannot spell are adapted (see above).

Set `useCamelCaseNames` to `true` to declare every Java name in camel case
instead: `pageNum`, `totalItems`, `getTotalItems()`, `setTotalItems(...)` and
the builder's `totalItems(...)`. What goes over the wire does not change: models
keep `@JsonProperty("total_items")`, and parameters keep
`@RequestParam(name = "page_num")`, so the JSON and the requests still use the
contract names.

```xml
<specFile>
  <filePath>openapi/openapi.yml</filePath>
  <useCamelCaseNames>true</useCamelCaseNames>
</specFile>
```

Switching it on renames the generated getters, setters and builder methods, so
code that uses them has to be updated once.

### Unknown enum values (useUnknownEnumValue)

Since 8.0.0 generated enums can have an extra `UNKNOWN` constant. Reading a
value that the contract does not list resolves to `UNKNOWN` instead of failing,
so a provider that adds a value to an enum no longer breaks the services that
consume it:

```java
public enum Status {
  ACTIVE("active"),
  BLOCKED("blocked"),
  UNKNOWN("UNKNOWN");
  ...
  @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
  public static Status fromValue(String value) { ... } // "retired" -> UNKNOWN
}
```

- `UNKNOWN` is written back as `"UNKNOWN"`, also for numeric enums, so a value
  that was not understood shows up as such instead of being hidden.
  `null` stays `null`.
- Numeric enums compare by value, so `4.40` matches `4.4`. Their `getValue()`
  returns `null` for `UNKNOWN`.
- If the contract already declares a value named `UNKNOWN` (`unknown`,
  `Unknown`...), that constant is the one unknown values resolve to, and no
  extra constant is added.
- The resolution lives in the enum itself (`@JsonCreator`), so it does not
  depend on how the `ObjectMapper` is configured. It works the same with
  Jackson 2 and Jackson 3.

#### Handling UNKNOWN in your code

Every enum with the fallback has `isUnknown()`, which is `true` only for the
constant that values outside the contract resolve to. Check it instead of
checking `getValue()` for `null`: in numeric enums `getValue()` returns `null`
for `UNKNOWN`, so code like `e.getValue().equals(x)` or `e.getValue().intValue()`
throws a `NullPointerException` there.

- **Converting a value to the enum.** Use the generated `fromValue` instead of a
  loop over `values()`. It matches the contract value exactly and returns
  `UNKNOWN` when nothing matches:

  ```java
  Code code = Code.fromValue(raw);
  if (code.isUnknown()) {
    // the value is not in the contract
  }
  ```

- **Looping over `values()` yourself** (for example to ignore case, or to keep
  returning `null` when nothing matches): skip `UNKNOWN` before calling
  `getValue()`. Otherwise an input `"UNKNOWN"` now matches the new constant.

  ```java
  for (Code c : Code.values()) {
    if (c.isUnknown()) {
      continue;
    }
    if (c.getValue().equals(raw)) {
      return c;
    }
  }
  return null;
  ```

- **Failing where the value matters.** Deserialization stays tolerant, so decide
  at the point that needs a known value, and fail there with a clear message
  instead of a `NullPointerException`:

  ```java
  if (dto.getCode().isUnknown()) {
    throw new IllegalStateException("Code outside the contract for order " + dto.getId());
  }
  ```

#### Which side gets it (since 8.2.0)

Tolerance only matters when reading JSON, so by default the fallback follows the
side that reads what someone else wrote:

| Generation                           | Default   | Why                                                                                 |
|--------------------------------------|-----------|-------------------------------------------------------------------------------------|
| OpenAPI with `callMode` (clients)    | `UNKNOWN` | Responses come from the provider, which may add values to its enums                 |
| OpenAPI without `callMode` (servers) | strict    | A request body outside the contract keeps being rejected with a 400                 |
| AsyncAPI (every section)             | `UNKNOWN` | Consumers read what others publish; suppliers only write their own messages        |

Set `useUnknownEnumValue` to force it either way on a spec file. `true` on a
server accepts a request body with a value outside the contract as `UNKNOWN`,
instead of rejecting it:

```xml
<specFile>
  <filePath>openapi/openapi.yml</filePath>
  <useUnknownEnumValue>true</useUnknownEnumValue>
</specFile>
```

In 8.0.0 and 8.1.0 the fallback was on for every spec file unless it was set to
`false`. From 8.2.0, a server that did not set it gets strict enums again, as in
7.x.

#### What stops compiling

With the fallback on, code that handles every constant of a generated enum has
to handle `UNKNOWN` too:

- A `switch` that lists every constant without a `default`.
- A MapStruct mapper from a generated enum to a domain enum: MapStruct requires a
  target for each source constant. Map `UNKNOWN` explicitly, to `null` or to a
  domain value:

  ```java
  @ValueMapping(source = "UNKNOWN", target = MappingConstants.NULL)
  DomainState toDomain(StateDTO state);
  ```

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

## Loading specifications from the plugin classpath

The plugin also searches its own classpath before falling back to the project
directories, so a contract can be named by its resource path. The artifact has to
be a dependency **of the plugin** — a project dependency is not on that
classpath — and relative `$ref`s are still resolved against the module directory,
so this only works for a single-file contract. For anything else, prefer
[Loading Specs from a Published Artifact](#loading-specs-from-a-published-artifact).

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

## Loading specifications from a remote URL (Apicurio Registry, HTTP)

`filePath` also accepts a remote URL (`http`, `https` or `file` scheme). When
it does, the spec is downloaded at generation time instead of being read from
the filesystem or the classpath. This works for both OpenAPI and AsyncAPI
specs. Remote fetches use bounded connect/read timeouts so an unresponsive
server cannot hang the build.

This is the mechanism to consume a spec published in an **Apicurio Registry**,
whose artifacts are served over HTTP. Point `filePath` at the artifact's
content endpoint, for example:

```xml
<specFile>
    <filePath>https://my-apicurio-host/apis/registry/v2/groups/default/artifacts/my-api</filePath>
    <apiPackage>com.sngular.apigenerator.openapi.api</apiPackage>
    <modelPackage>com.sngular.apigenerator.openapi.model</modelPackage>
</specFile>
```

Gradle:

```groovy
filePath = 'https://my-apicurio-host/apis/registry/v2/groups/default/artifacts/my-api'
```

### Authenticated registries

For a protected registry (for example an Apicurio Registry with security
enabled), credentials are read from **system properties** (preferred) or
**environment variables** — never from the build files — and sent as request
headers on `http`/`https` fetches. Each mechanism has a system property and an
equivalent environment variable:

- **Bearer token**: `scs.multiapi.remote.token` /
  `SCS_MULTIAPI_REMOTE_TOKEN`.
- **Basic auth**: `scs.multiapi.remote.user` + `scs.multiapi.remote.password`
  (env `SCS_MULTIAPI_REMOTE_USER` / `SCS_MULTIAPI_REMOTE_PASSWORD`).
- **Custom header**: `scs.multiapi.remote.header.name` +
  `scs.multiapi.remote.header.value` (env `SCS_MULTIAPI_REMOTE_HEADER_NAME` /
  `SCS_MULTIAPI_REMOTE_HEADER_VALUE`), e.g. an `X-Registry-ApiKey`.
- **Restrict credentials to a host**: `scs.multiapi.remote.host`
  (env `SCS_MULTIAPI_REMOTE_HOST`).

A bearer token takes precedence over basic auth; the custom header is
additive. Example (bearer token from the CI environment):

```bash
export SCS_MULTIAPI_REMOTE_TOKEN="$APICURIO_TOKEN"
export SCS_MULTIAPI_REMOTE_HOST="my-apicurio-host"   # optional but recommended
mvn generate-sources
```

Notes:

- Set `scs.multiapi.remote.host` to the registry host so the token is sent
  **only** to that host and never leaked to a different host reached through
  an external `$ref` or a cross-host redirect.
- Provide credentials via CI secrets / environment variables; they are never
  logged.
- External `$ref`s are resolved relative to the spec's URL when the spec is
  remote.
- For an `https` registry using an internally-issued or self-signed
  certificate, the certificate must be trusted by the JVM running the build
  (for example imported into its truststore); validation is not disabled.

## Loading Specs from a Published Artifact

When your contracts are published as an artifact — the usual setup when a
producer and its consumers must not each keep their own copy — point at them by
coordinates instead of by path. The artifact is fetched through the repositories
your build is already configured with, including private ones, so it does not
have to be a dependency of the project.

`filePath` then means *the path inside the artifact*.

### Maven configuration for a published artifact

```xml
<specFile>
  <filePath>contracts/api.yml</filePath>
  <fromGroupId>com.company</fromGroupId>
  <fromArtifactId>api-specs</fromArtifactId>
  <fromVersion>1.0.0</fromVersion>   <!-- optional -->
  <apiPackage>com.example.consumer.api</apiPackage>
  <modelPackage>com.example.consumer.model</modelPackage>
  <callMode>false</callMode>
</specFile>
```

### Gradle configuration for a published artifact

```groovy
openapimodel {
  specFile {
    filePath = 'contracts/api.yml'
    fromGroupId = 'com.company'
    fromArtifactId = 'api-specs'
    fromVersion = '1.0.0'   // optional
    apiPackage = 'com.example.consumer.api'
    modelPackage = 'com.example.consumer.model'
  }
  overWriteModel = true
}
```

The same three fields work on an `asyncapimodel` spec file and on the
`asyncapi-generation` goal.

### Two contracts, two artifacts

A service that implements one API and calls another, where both contracts happen
to sit at the same path inside their own artifact:

```xml
<specFiles>
  <specFile>
    <filePath>contracts/api.yml</filePath>
    <fromGroupId>com.company</fromGroupId>
    <fromArtifactId>api-warehouse</fromArtifactId>
    <apiPackage>com.example.infra.rest.api</apiPackage>
    <callMode>false</callMode>
  </specFile>
  <specFile>
    <filePath>contracts/api.yml</filePath>
    <fromGroupId>com.company</fromGroupId>
    <fromArtifactId>api-logistics</fromArtifactId>
    <apiPackage>com.example.infra.rest.client.logistics</apiPackage>
    <callMode>true</callMode>
  </specFile>
</specFiles>
```

Naming the artifact per spec is what keeps the two apart; a classpath lookup
could not.

### What to expect

- `fromGroupId` and `fromArtifactId` go together. Setting only one fails with a
  message saying so, rather than quietly falling back to the filesystem.
- `filePath` is the path *inside* the artifact, and it can be omitted when the
  artifact follows the conventional layout described below. Failing that, an
  artifact carrying a single contract is used too; with several and none at the
  conventional path it is required, and the build lists them rather than picking
  for you.
- Omit `fromVersion` and the version already declared by the build is used, so
  the artifact stays pinned in one place.
- Multi-file contracts work: the artifact is unpacked under the build directory
  (`target/generated-resources/multiapi-specs` in Maven) so a `$ref` to another
  file inside it resolves like any relative reference. Nothing is written to your
  sources.
- A wrong `filePath` fails listing the spec files the artifact does carry.

### The conventional contract location

Put the contract at **`contract/openapi.yml`** — or `contract/asyncapi.yml` for
`asyncapi-generation` — and `filePath` can be left out altogether. The same
convention applies whether the contract sits in the module or at the root of a
published artifact, and the `.yaml` spelling is accepted too:

```text
your-module/                       api-specs artifact/
└── contract/                      └── contract/
    ├── openapi.yml                    ├── openapi.yml
    └── schemas/                       └── schemas/
        └── user.yml                       └── user.yml
```

```xml
<specFile>
  <apiPackage>com.example.api</apiPackage>
  <modelPackage>com.example.api.model</modelPackage>
</specFile>
```

Configure `filePath` when the contract is somewhere else, when a module or an
artifact holds more than one, or when it is behind a URL.

See [Loading specs](docs/LOADING_SPECS.md) for every way a contract can be
located, and [Architecture](docs/ARCHITECTURE.md) for how resolution fits into
the generation pipeline.
