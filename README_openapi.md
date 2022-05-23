# Openapi Multifile Maven Plugin

## 📜 Summary:

OpenApi Multifile Maven Plugin allows us to create multiple apis with one maven clean install execution.

## 🚀 Getting Started

In order to get this plugin working, you need the following things installed in your computer:

- Java 11 Version
- Maven

Depending on the approach with which you are going to use the plugin, other dependencies will be necessary, for example:

- spring-boot-starter-webflux, in case you want to implement an API with responses in Mono/Flux Reactor types or use them for external calls through Spring WebClient.

After you have these installed, you need to add this plugin in your pom.xml file. Here is an example of a basic configuration:

```xml
<plugin>
  <groupId>com.corunet</groupId>
  <artifactId>openapi-multifile-maven-plugin</artifactId>
  <version>1.0.0</version>
  <executions>
    <execution>
        <goals>
            <goal>configure</goal>
        </goals>
        <configuration>
            <fileSpecs>
                <fileSpec>
                    <inputSpec>${project.basedir}/src/main/resources/api/api.yml</inputSpec>
                    <apiPackage>com.corunet.challenge.gameserver.api</apiPackage>
                    <modelPackage>com.corunet.challenge.gameserver.api.model</modelPackage>
                    <modelNameSuffix>DTO</modelNameSuffix>
                </fileSpec>
            </fileSpecs>
        </configuration>
    </execution>
  </executions>
</plugin>
```

## 🧑🏻 Initial Considerations

Before using this plugin we have to warn that not all the complexity and support offered by the use of swagger.io yml files is supported.

We establish here some of these options that are not yet supported and that will be added to this plugin as time goes by and the existing need among users.

- The use of common parameters for all the operations of the same path element of the .yml file.

- The use of parameters defined in the component element by reference.

- The use of parameters with content tag.

- Using Multiple Authentication Types within the security options both at an operational and general level.

- The use of OAuth 2 and OpenID Connect Discovery Authentication Types.


## 🧑🏻‍💻 Usage

This plugin allows us to create multiple apis with just one maven clean install execution, in this way the user can configure several fileSpecs tags with different uses, thus generating Apis in the two possible modes: send or receive calls, depending on the options of configuration selected in said fileSpecs.

```xml
<configuration>
    <fileSpecs>
        <fileSpec>
            <inputSpec>${project.basedir}/src/main/resources/api/api.yml</inputSpec>
            <apiPackage>com.corunet.challenge.gameserver.api</apiPackage>
            <modelPackage>com.corunet.challenge.gameserver.api.model</modelPackage>
            <modelNameSuffix>DTO</modelNameSuffix>
        </fileSpec>
    </fileSpecs>
</configuration>
```

To customize these fileSpecs tags we are going to specify them inside the configuration tag, we must declare the fileSpecs tag that contains all files that will be used. Each fileSpec has their own configuration:

| Name      | Description                                                                                                                                                                                         | Example                                           |
| ---------- |-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| inputSpec  | Path where the yaml is located                                                                                                                                                                      | ${project.basedir}/src/main/resources/api/api.yml |
| apiPackage | Path where the api interface will be located                                                                                                                                                        | com.corunet.challenge.gameserver.api              |
| modelPackage | Path where the models will be located                                                                                                                                                               | com.corunet.challenge.gameserver.api.model        |
| modelNamePrefix | Prefix that will be used ahead of every model´s name                                                                                                                                                | Api                                               |
| modelNameSuffix | Suffix that will be used ahead of every model´s name                                                                                                                                                | DTO                                               |
| callMode   | Boolean value to decide if you want to generate the api for external calls. **Use RestClient by default. It´s initialized to false by default**                                                     | false                                             |
| useTagsGroup | Boolean value to decide if using tags instead of an URL for group the API. **It´s initialized to false by default**                                                                                 | false                                             |
| useLombokModelAnnotation | Boolean value to decide if you want your models with Lombok or not   **It´s initialized to false by default**                                                                                       | false                                             |
| isReactive | Boolean value to decide if you want to generate the api with responses in Mono/Flux Reactor types. If callmode = true use WebClient instead of RestClient. **It´s initialized to false by default** | false                                             |

As the configuration options already indicate, the data model will also be created within the specified path.This model will be created with the indicated prefixes and suffixes and the instances and imports will be made to that model within the corresponding Api.

Only one property is configured outside the fileSpecs, the path where the RestClient and the WebClient will be located, if this option is set in any of the fileSpecs.

| Name      | Description                                            | Example                                 |
| ---------- |--------------------------------------------------------|-----------------------------------------|
| clientPackage  | Path where the RestClient and/or WebClient are located | com.corunet.challenge.gameserver.client |

We must clarify that the options to make calls are configured under the RestClient or WebClient specifications as indicated above in the configuration options. If several of the APIs to be generated are defined under the same call option, a single RestClient/Webclient will be generated for all of them, which is initialized with the specific options needed within the class that defines each API.

## Testing

This plugin uses `itf-maven-plugin` for testing purposes, so the tests are defined following it's documentation.

That implies that for each test defined in the `BaseTest` class, has it's own related folder under `test/resources/.../BaseTest`, where there will be various needed files for the test execution, as a `pom.xml`, a `api-test.yml` and the expected result files.

Also, every test is annotated with
```java
@MavenGoal("groupId:artifactId:version:goal")
```
to assure that the execution of the test will be done with an specific version of the plugin.

Making the test execution linked to the plugin version, declaring it in the POM file and the annotation, makes neccesary to change it on both the file and annotation for each test every time the version of the plugin is modified.


## 🌐 RoadMap:

- Support Arrays, Maps and other complex parameters
- Add max level of nested collections in parameters/schemas model (max of two)
- Add Spring Cloud OpenFeign callMode 