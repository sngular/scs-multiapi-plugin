# Additional Information

Guides, references and support for the SCS MultiAPI Plugin. Every link here
points at a page that exists.

## Documentation

- **[Getting Started](Getting-Started)** — installing the plugin and generating
  from your first contract.
- **[README][readme]** — the full configuration reference for both goals, both
  build tools and every option.
- **[Loading specs](LOADING_SPECS)** — where a contract can live (this module, a
  published artifact, a URL or a registry, the plugin classpath) and which to
  pick.
- **[Architecture](ARCHITECTURE)** — the generation pipeline, module boundaries
  and where to hook in when changing the plugin.
- **[Spring-Kafka Integration](SPRING_KAFKA_INTEGRATION)** — AsyncAPI with
  Spring Cloud Stream and Kafka end to end.
- **[OpenAPI 3.1 roadmap](OPENAPI31_ROADMAP)** — what of OpenAPI 3.1 and JSON
  Schema 2020-12 is supported, and what is planned.
- **[CONTRIBUTING][contributing]** — development setup, code style and the pull
  request process.

## By use case

**Building a REST API** — [Getting Started](Getting-Started), then the
[OpenApi Generator][openapi-guide] section of the README.

**Building a message-driven system** — [Getting Started](Getting-Started), then
[Spring-Kafka Integration](SPRING_KAFKA_INTEGRATION) and the
[AsyncApi Generator][asyncapi-guide] section of the README.

**Sharing contracts between services** — [Loading specs](LOADING_SPECS). Publish
the contracts as an artifact and point each consumer at its coordinates.

**Changing the plugin itself** — [Architecture](ARCHITECTURE), then
[CONTRIBUTING][contributing].

## Feature matrix

| Feature | OpenAPI | AsyncAPI |
| --- | --- | --- |
| REST API generation | ✅ | ❌ |
| Message producers and consumers | ❌ | ✅ |
| Kafka and RabbitMQ bindings | ❌ | ✅ |
| Validation annotations | ✅ | ✅ |
| Lombok support | ✅ | ✅ |
| Reactive types | ✅ | ✅ |
| Spec from the plugin classpath | ✅ | ✅ |
| Spec from HTTP / Apicurio Registry | ✅ | ✅ |
| Spec from a published artifact | ✅ | ✅ |

Everything above works on both build tools and on Spring Boot 2, 3 and 4.

## External references

- [OpenAPI specification](https://spec.openapis.org/oas/latest.html)
- [AsyncAPI specification][asyncapi-spec]
- [Spring Cloud Stream](https://spring.io/projects/spring-cloud-stream)
- [Maven plugin on Maven Central][maven-central]
- [Gradle plugin on the Gradle Plugin Portal][gradle-portal]

## FAQ

**Can I use OpenAPI and AsyncAPI in the same project?** Yes. The two goals are
independent; configure one, the other, or both.

**Is the Gradle plugin equivalent to the Maven one?** Yes, option for option.
Maven configures through `<specFile>` elements, Gradle through the
`openapimodel` / `asyncapimodel` extensions.

**My contract is in a JAR and the build says it cannot read the file.** The
coordinates are what tell the plugin to look inside an artifact. See
[Loading specs](LOADING_SPECS) — a project dependency alone is not enough.

**Can I customise the generated code?** Package and class naming, the output
folder, Lombok, validation and reactive types are all configurable; see the
[README][readme]. For behaviour, implement the generated interfaces rather than
editing generated sources, and keep `overWriteModel` on.

**Does it work in CI?** Yes, it is an ordinary Maven or Gradle plugin. A contract
loaded from an artifact or a registry needs the same credentials there as any
other dependency.

## Getting help

- [GitHub Issues][issues] — include the plugin version, build tool, Java
  version, the full error and a minimal contract that reproduces it.
- [GitHub Discussions][discussions]

---

**Back to**: [Home](Home) | [Getting Started](Getting-Started)

[readme]: https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md
[openapi-guide]: https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md#openapi-generator
[asyncapi-guide]: https://github.com/sngular/scs-multiapi-plugin/blob/main/README.md#asyncapi-generator
[contributing]: https://github.com/sngular/scs-multiapi-plugin/blob/main/CONTRIBUTING.md
[asyncapi-spec]: https://www.asyncapi.com/docs/reference/specification/latest
[maven-central]: https://mvnrepository.com/artifact/com.sngular/scs-multiapi-maven-plugin
[gradle-portal]: https://plugins.gradle.org/plugin/com.sngular.scs-multiapi-gradle-plugin
[issues]: https://github.com/sngular/scs-multiapi-plugin/issues
[discussions]: https://github.com/sngular/scs-multiapi-plugin/discussions
