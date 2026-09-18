# Architecture

How the plugin turns an OpenAPI or AsyncAPI contract into Java sources, and where
to hook in when you change it.

This document tracks `main`. It is not versioned: when behaviour changes, this
page changes with it, and anything worth remembering about a past release lives
in [Version history](#version-history) at the bottom.

## Modules

- **`multiapi-engine`** (Java 21) — all generation logic. Knows nothing about
  Maven or Gradle.
- **`scs-multiapi-maven-plugin`** (Java 21) — the `openapi-generation` and
  `asyncapi-generation` mojos, adapting Maven to the engine. Shaded, so the
  engine travels inside the plugin artifact.
- **`scs-multiapi-gradle-plugin`** (Groovy) — `openApiTask` / `asyncApiTask` and
  the `openapimodel` / `asyncapimodel` extensions.

The build tools own everything environmental — where the build directory is, how
to reach a repository, what the project declares — and hand the engine plain
values. That boundary is why the engine exposes small interfaces such as
`SpecArtifactResolver` rather than reaching for a build API itself.

## Where a contract comes from

`filePath` is resolved in this order, the first hit winning
(`SchemaUtil.readFile`, `PathUtil`):

1. **An artifact**, when the spec declares `fromGroupId` + `fromArtifactId`.
   Resolved and unpacked before anything reads it — see below.
2. **A remote URL** (`http`, `https`, `ftp`, `file`), for a registry such as
   Apicurio. Credentials come from system properties or environment variables;
   see the README section on remote specs.
3. **The plugin classpath**, for a contract shipped in an artifact added to the
   *plugin's* own `<dependencies>` (Maven) or the buildscript classpath
   (Gradle). A project dependency is not on that classpath and will not be
   found. Relative `$ref`s are resolved against the module directory, not
   against the artifact, so this route only works for a single-file contract.
4. **The module's filesystem**, relative to the project base directory.

### Specs published as an artifact

Teams that publish their contracts as an artifact point at it by coordinates
instead of by path:

```xml
<specFile>
  <filePath>contracts/api.yml</filePath>   <!-- path INSIDE the artifact -->
  <fromGroupId>com.company</fromGroupId>
  <fromArtifactId>api-specs</fromArtifactId>
  <fromVersion>1.0.0</fromVersion>          <!-- optional -->
</specFile>
```

The flow, all of it before the document is parsed:

```text
SpecFile (fromGroupId/fromArtifactId/fromVersion)
   │
   ├─ ExternalSpecSource.validateDependencyCoordinates()   both fields, or neither
   │
   ├─ SpecArtifactResolver.resolveArtifact(...)  ──► the artifact file
   │      Maven  : MavenSpecArtifactResolver   (Maven Resolver: settings.xml
   │               repositories, mirrors, proxies, credentials)
   │      Gradle : GradleSpecArtifactResolver  (a detached configuration)
   │      neither: LocalRepositorySpecArtifactResolver (local repository only)
   │
   ├─ DependencySpecMaterializer.materialize(...)
   │      unpacks once per artifact into
   │      <build-dir>/generated-resources/multiapi-specs/<groupId>/<artifactId>/<artifact-file>/
   │
   └─ filePath is rewritten to the extracted copy
```

Unpacking, rather than reading entries straight out of the artifact, is what
makes a **multi-file** contract work: by the time the pipeline sees it, a
`$ref: './schemas/user.yml'` is an ordinary relative file like any other, so
base-URI computation, nested reference resolution and the model builder need no
special case. A JAR-aware path would have to re-implement each of those.

Details worth knowing:

- Extraction is cached per resolved artifact through a marker file holding the
  artifact's path and timestamp, so repeated builds and several specs from the
  same artifact unpack it once.
- Entries are checked against the destination before being written, and a
  `filePath` that would escape the extracted content is rejected.
- `.class` entries are skipped: a spec artifact carries resources.
- When `fromVersion` is omitted, the version already declared by the consuming
  build is used — the project's dependencies, then its dependency management.
- A `filePath` that is not in the artifact fails with the spec files the
  artifact does carry, because that error is almost always a wrong path.
- `filePath` may be omitted when the artifact carries exactly one contract. A
  contract is a document declaring a top-level `openapi` or `asyncapi` field —
  counting spec *files* would not do, because a multi-file contract ships its
  fragments beside the root document and they are `.yml` files too, and the
  marker also keeps an artifact publishing both kinds from feeding the wrong one
  to the wrong generator. With several, choosing one would be a guess at which
  API to generate, so it fails listing them.

## OpenAPI pipeline

`OpenApiGenerator.processFileSpec` runs this per spec file:

1. `resolveSpecFile` — resolve the artifact, if any, and rewrite `filePath`.
2. `OpenApiUtil.getPojoFromSpecFile` — read and parse the document.
3. `OpenApiUtil.mergeWebhooksIntoPaths` — 3.1 `webhooks` become `paths` entries
   so one path pipeline serves both, each operation defaulting its `tags` to the
   webhook name.
4. `resolveSpecBaseUri` + `OpenApiUtil.solvePathRefs` — dereference Path Items
   declared as a `$ref` to another file, which would otherwise disappear from
   generation.
5. `OpenApiUtil.mapApiGroups` — group operations by tag or by URL, one generated
   interface per group.
6. `MapperPathUtil.mapPathObjects` — the template model: parameters, request
   bodies, responses, security.
7. `MapperAuthUtil` — authentication objects, and the client/auth templates when
   `callMode` is on.
8. `OpenApiUtil.processPaths` + `ModelBuilder` — the schema map behind the
   generated models.
9. `TemplateFactory` — render the FreeMarker templates.

### Response wrappers

`ResponseWrapperHandler` is the single authority on whether a response needs a
generated wrapper class, what it is called and what type the interface exposes.
Both the model side (`OpenApiUtil`) and the interface side (`MapperPathUtil`) ask
it, which is what keeps them from disagreeing — an interface referring to an
`InlineResponse200*` class that the model side never generated used to be the
classic symptom.

The rules: an inline object, a composed type (`allOf`/`anyOf`/`oneOf`) or an
array with inline items gets a wrapper; a direct `$ref` or an array of `$ref`
items does not, and is used as the referenced type or `List<RefType>`; nested
arrays recurse.

## AsyncAPI pipeline

`AsyncApiGenerator.processFileSpec` resolves the spec the same way, reads the
document, then dispatches on its `asyncapi` version through
`AsyncApiHandlerFactory` to `AsyncApi2Handler` or `AsyncApi3Handler`, which share
`BaseAsyncApiHandler`. Handlers map channels and operations to
supplier/consumer/streamBridge bindings and render through
`CommonTemplateFactory`.

## Adding to the engine

- **A new spec source** — implement `SpecArtifactResolver` if it is another kind
  of repository, or extend `SchemaUtil.readFile` if it is another kind of
  location. Keep build-tool APIs out of the engine.
- **A new schema construct** — `ModelBuilder` for the model side, `MapperPathUtil`
  for the interface side, and `ResponseWrapperHandler` if it changes what a
  response exposes. Changing one of the first two without the third is how the
  two sides drift apart.
- **A new generated file** — a FreeMarker template under
  `src/main/resources/templates`, rendered from `TemplateFactory`.
- **Tests** — `OpenApiGeneratorTest` and `AsyncApiGeneratorTest` compare
  generated output against the fixtures in `src/test/resources`, which is the
  fastest way to pin behaviour. The Maven plugin has `itf` integration tests and
  the Gradle plugin has GradleTestKit ones for anything that depends on the
  build tool.

## Version history

- **7.1.3** — `filePath` defaults to the artifact's single contract when it has
  one. `fromGroupId`/`fromArtifactId`/`fromVersion` actually resolve the
  artifact, through the repositories the build is configured with, for both
  OpenAPI and AsyncAPI. Before this the fields were accepted and ignored, so the
  contract was still read from the module's filesystem. In Gradle they moved from
  the task to each `specFile`, matching Maven and this page.
- **7.1.0** — the fields were introduced.
- **7.0.0** — response wrapper decisions were unified into
  `ResponseWrapperHandler`; before that the model side and the interface side
  each decided for themselves and could disagree.
