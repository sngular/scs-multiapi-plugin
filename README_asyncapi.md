# AsyncAPI SCS Maven Plugin

This is a Maven plugin designed to help developers automatizing the creation of consumers and publishers for Spring Cloud Stream, based on an YML file containing the definitions created by AsyncAPI.

# Index

- [AsyncAPI SCS Maven Plugin](#asyncapi-scs-maven-plugin)
- [Index](#index)
- [Usage](#usage)
  - [Installation](#installation)
  - [Using in other projects](#using-in-other-projects)
  - [How targetPackage is setted?](#how-targetpackage-is-setted)
  - [How modelPackage is setted?](#how-modelpackage-is-setted)
- [Class Generation](#class-generation)
  - [Consumer and Supplier classes](#consumer-and-supplier-classes)
    - [Method interfaces](#method-interfaces)
    - [Mapper](#mapper)
        - [Mapper](#mapper-1)
        - [Implementation](#implementation)
  - [Stream Bridge class](#stream-bridge-class)
- [Information for developers](#information-for-developers)
  - [Testing](#testing)

# Usage

To be able to use this plugin in your own project, you need to install it locally.

## Installation
For this, first clone or download the repository to your computer, and once it's ready, execute `mvn clean install`.

## Using in other projects
Now that the plugin is installed, you can use it in other projects, including it in their's `pom.xml` file.

The plugin defined `phase` and `goal` parameters are expected to be *generate-sources* and *configure*, as they are the only values for which the plugin is designed.

```xml
<plugin>
  <groupId>com.corunet</groupId>
  <artifactId>asyncapi-scs-maven-plugin</artifactId>
  <version>1.0.0-SNAPSHOT</version>
  <executions>
    <execution>
      <phase>generate-sources</phase>
      <goals>
        <goal>configure</goal>
      </goals>
    </execution>
  </executions>
  <configuration>
    <fileParameterObject>
      <param>
        <filePath>PATH_TO_YML</filePath>
      </param>
      <param>
        <filePath>PATH_TO_YML</filePath>
        <consumer>
          <ids>publishOperation</ids>
          <classNamePostfix>MY_CONSUMER_CLASS</classNamePostfix>
          <entitiesPostfix>DTO</entitiesPostfix>
          <targetPackage>com.corunet.scsplugin.business_model.model.event.consumer</targetPackage>
          <modelPackage>com.corunet.scsplugin.business_model.model.event</modelPackage>
        </consumer>
        <supplier>
          <ids>subscribeOperation</ids>
          <targetPackage>com.corunet.scsplugin.business_model.model.event.producer</targetPackage>
          <modelPackage>com.corunet.scsplugin.business_model.model.event</modelPackage>
        </supplier>
      </param>
    </fileParameterObject>
  </configuration>
</plugin>

```

As you can see in the example above, there is a main parameter **fileParameterObject** that receives a list of **param** attributes groups, so you can set as many YML files as you want.

**fileParameterObject** could be configured in two different ways:

1. The first one is to configure only the YML file. This is made using the **filePath** parameter, that expects to receive the path to the file. Using the plugin in this way, you can't configure the model package or the target package in the pom file, neither other options, so they will be configured as its explained in [targetPackage](#how-targetPackage-is-setted) and [modelPackage](#how-modelPackage-is-setted) sections.  
This way it's limited to the usage of Consumer and Supplier methods.

```xml
<param>
    <filePath>PATH_TO_YML</filePath>
</param>
```

1. The second one is to configure the YML file with the consumers, supplier producers and streamBrige producers that you want to generate.

``````xml
<param>
    <filePath>PATH_TO_YML</filePath>
    <consumer>
        <ids>publishOperation</ids>
        <classNamePostfix>MY_CONSUMER_CLASS</classNamePostfix>
        <entitiesPostfix>DTO</entitiesPostfix>
        <targetPackage>com.corunet.scsplugin.business_model.model.event.consumer</targetPackage>
        <modelPackage>com.corunet.scsplugin.business_model.model.event</modelPackage>
    </consumer>
    <supplier>
        <ids>subscribeOperation</ids>
        <targetPackage>com.corunet.scsplugin.business_model.model.event.producer</targetPackage>
        <modelPackage>com.corunet.scsplugin.business_model.model.event</modelPackage>
    </supplier>
    <streamBridge>
        <ids>streamBridgeOperation</ids>
        <targetPackage>com.corunet.scsplugin.business_model.model.event.producer</targetPackage>
        <modelPackage>com.corunet.scsplugin.business_model.model.event</modelPackage>
    </streamBridge>
</param>
``````

As you can see in the example above, there are three blocks of parameters that can be configured in the plugin.

- **filePath**: This parameter works in the same way as in the first option.
- **consumer**, **supplier** and **streamBridge**: They are both configured in the same way and can receive the same parameters. These parameters are:
  - **ids**: With this parameter you can set the operationId that you want to be generated as subscriber or publisher. If this parameter is not defined for the `consumer` section, all the subscribe operations defined in the YML file, will be generated. If only one of `supplier` and `streamBridge` sections are defined, and this parameter is not defined inside it, all the publish operations defined in the YML file will be generated. If both `supplier` and `streamBridge` sections are defined, it`s needed to define which operations belongs to each category.
  - **classNamePostfix**: This parameter receive the name of the class that it's going to be generated containing the Beans. This parameter is optional, and by default the classes will be called `Producer`, `StreamBridgeProducer` and `Subscriber`.
  - **entitiesPostfix**: With this parameter you can set the postfix that is going to be used in the entities of the generated classes. For example if you set this to `DTO`, and there is a class named `EntityClass`, it will result as `EntityClassDTO`. This parameter is optional.
  - **targetPackage**: This parameter receive a package name, where the generated classes will be generated. This parameter is optional. Check [how the targetPackage is setted](#how-targetPackage-is-setted) for more information about how this parameter works, and the values it could have.
  - **modelPackage**: This parameter receive a package name, where the entities used for the generated classes are defined. As it's explained in the [Mapper Section](#mapper), those entities are usually auto-generated, so the plugin expects the modelPackage to be the package where them are included.
    **Note that the plugin doesn't create the entities neither checks their existence**, it takes their names from the YML file and assume that they are created by the user. As the previous parameter, this is also optional. Check [how the modelPackage is setted](#how-modelPackage-is-setted) for more information about how his parameter works, and the values it could have.


The configuration of `consumer`, `supplier` and `streamBridge` are independent. If only one of them is configured in the pom file, only that one will be generated.

## How targetPackage is setted?
The target package could be set in three different ways.
- **User definition**: The user provides a package name using the parameter in the pom.xml file.
- **GroupID from YML**: If the user doesn't provide a package name, the plugin will try to use the `groupId` attribute from the YML file that is in use.
- **Default package name**: If neither of the previous options were given, the plugin will use a default package name, that is stablished as `com.corunet.scsplugin`.

## How modelPackage is setted?
The model package could be set in four different ways.
- **User definition**: The user provides a package name using the parameter in the pom.xml file.
- **Namespace from YML**: If the user doesn't provide a package name, the plugin will check if the entity name definition in the YML file, includes a complete package name.
```yaml
order/createCommand:
    subscribe:
      operationId: "subscribeOperation"
      message:
        $ref: '#/components/messages/com.corunet.scsplugin.example.model.CreateOrder'
```
- **Namespace from Avro**: If the user doesn't provide a package name, and the entity is defined by an Avro Schema, the plugin will check for a `namespace` attribute defined in the Avro file, and if there is, it will use it. The plugin expects to receive a relative path from the `yml` file folder.
```yaml
order/created:
    publish:
      operationId: "publishOperation"
      message:
        $ref: 'path_to_Avro_file'
```
- **Default package name**: If neither of the previous options were given, the plugin will use a default package name, that is stablished as `com.corunet.scsplugin.model`.

# Class Generation

## Consumer and Supplier classes
Those are a pair of classes, separated by the directionality of the messages. They came from the plugin fully implemented by making reference to the interfaces of the next section. Their names could be modified using the `classNamePostfix` parameter specified on the [Usage section](#using-in-other-projects), being by default **Producer** and **Subscriber**.

```java
@Configuration
public class StreamTopicListenerConsumer {

    private final ISubscribeOperation subscribeOperation;

    protected StreamTopicListenerConsumer(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrder> consumerSubscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }
}
```

This sample class, is related to the previosly used YML file, and in it you could see that it came fully implemented, based on the related Interface that lets the personalitation and implementation to the user. Also, in this example is possible to see how the YML attribute 'operationId' is used to name the methods as `Consumer'OperationId'` or `Publisher'OperationId'`.

### Method interfaces
Those are a group of interfaces that are related to the previous seen classes. There are as many as operations are defined in the YML file, and in the previous classes, so there is only one operation defined in each interface.

This layer is the only one that needs work by the end user, so it needs to implement these interfaces.

This interfaces are named following the "I*OperationId*" pattern, where 'OperationId' comes from the YML file definition of the channels section. Also the method is named as 'OperationId' as well as on the classes in the above section.

```java
public interface ISubscribeOperation {

  void subscribeOperation(CreateOrder value);
}
```

### Mapper
The entities used for the definitions both on the previous seen classes and this interfaces, are auto-generated entities, based on the same YML file. Because of that, they need to be mapped to a user defined entity using a mapper utility class.

This mapper must be defined by the user on it's own way to improve the personalitation capabilities of the plugin.

Down here you have an example of the mapper utility class as well as an simple class implementing the interface defined above.

##### Mapper

```java
@Mapper
public interface Mapper {
  Order map(com.corunet.scsplugin.business_model.model.event.Order value);
}
```

##### Implementation

```java
@Component
public class subscribeOperation implements ISubscribeOperation {
  private final Mapper mapper;

  public subscribeOperation(final Mapper mapper) {this.mapper = mapper;}

  @Override
  public void subscribeOperation(final Order value) {
    com.corunet.scsplugin.business_model.model.Order orderMapped = mapper.map(value);
    //TODO: implement the functionality
  }
}
```

## Stream Bridge class

In this case, there is only one class where all the selected operations will be included. It's name could be modified using the `classNamePostfix` parameter specified on the [Usage section](#using-in-other-projects), being by default **StreamBridgeProducer**.

```java
@Configuration
public class StreamBridgeProducer {

    private StreamBridge streamBridge;

    public void streamBridgeOperation(CreateOrder createOrder){
        streamBridge.send("order-createCommand", createOrder);
    }
}
```
This sample class, is related to the previosly used YML file, and in it you could see that it came fully implemented.

Also, it's important to note that using Stream Bridge, the *topic* where the messages are going to be sent is included in the auto generated class. This topic cames from the YML file, taken from the channel identifier, as specified in the AsyncAPI documentation.

```yaml
channels:
  order/created:
    publish:
      operationId: "publishOperation"
      message:
        $ref: '#/components/messages/OrderCreated'
  order-createCommand:
    subscribe:
      operationId: "streamBridgeOperation"
      message:
        $ref: '#/components/messages/CreateOrder'
```

Due to the limitations on topics naming, the identifier of the channels that are going tu be used as Stream Bridge publishers, **only could include** `-` or `.` **as separators**, slash `/` is not allowed.

# Information for developers

## Testing

This plugin uses `itf-maven-plugin` for testing purposes, so the tests are defined following it's documentation.

That implies that for each test defined in the `BaseTest` class, has it's own related folder under `test/resources/.../BaseTest`, where there will be various needed files for the test execution, as a `pom.xml`, a `event-api.yml` and the expected result files. 

Also, every test is annotated with 
```java
@MavenGoal("groupId:artifactId:version:goal")
```
to assure that the execution of the test will be done with an specific version of the plugin.

Making the test execution linked to the plugin version, declaring it in the POM file and the annotation, makes neccesary to change it on both the file and annotation for each test every time the version of the plugin is modified.
