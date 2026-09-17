# Spring-Kafka Integration Guide

## Overview

This guide explains how to integrate the SCS MultiAPI Plugin with Spring-Kafka to implement message publishing and consuming in your Spring Boot application.

The plugin generates three types of classes for AsyncAPI specifications:
1. **Consumer** classes with `@Bean` methods returning `Consumer<T>`
2. **Supplier** classes with `@Bean` methods returning `Supplier<T>`  
3. **StreamBridge** classes for direct message publishing

## Quick Example

### Step 1: Define AsyncAPI Specification

```yaml
asyncapi: 2.3.0
info:
  title: Order Service
  version: 1.0.0
servers:
  kafka:
    url: localhost
    protocol: kafka
    protocolVersion: 0.9.1
channels:
  orders.created:
    publish:
      operationId: publishOrderCreated
      message:
        $ref: '#/components/messages/OrderCreated'
    subscribe:
      operationId: subscribeOrderCreated
      message:
        $ref: '#/components/messages/OrderCreated'
components:
  messages:
    OrderCreated:
      payload:
        $ref: '#/components/schemas/Order'
  schemas:
    Order:
      type: object
      properties:
        id:
          type: string
        amount:
          type: number
      required:
        - id
        - amount
```

### Step 2: Configure the Plugin

**Maven (`pom.xml`)**:
```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>6.7.8</version>
  <executions>
    <execution>
      <phase>generate-sources</phase>
      <goals>
        <goal>asyncapi-generation</goal>
      </goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>src/main/resources/asyncapi/order-api.yml</filePath>
            <consumer>
              <ids>subscribeOrderCreated</ids>
              <apiPackage>com.example.order.event.consumer</apiPackage>
              <modelPackage>com.example.order.event.model</modelPackage>
            </consumer>
            <supplier>
              <ids>publishOrderCreated</ids>
              <apiPackage>com.example.order.event.producer</apiPackage>
              <modelPackage>com.example.order.event.model</modelPackage>
            </supplier>
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
</plugin>
```

**Gradle (`build.gradle`)**:
```groovy
plugins {
  id 'com.sngular.scs-multiapi-gradle-plugin' version '6.7.8'
}

asyncapimodel {
  specFile {
    {
      filePath = 'src/main/resources/asyncapi/order-api.yml'
      consumer {
        ids = 'subscribeOrderCreated'
        apiPackage = 'com.example.order.event.consumer'
        modelPackage = 'com.example.order.event.model'
      }
      supplier {
        ids = 'publishOrderCreated'
        apiPackage = 'com.example.order.event.producer'
        modelPackage = 'com.example.order.event.model'
      }
    }
    overWriteModel = true
  }
}
```

### Step 3: Add Spring-Kafka Dependency

```xml
<dependency>
  <groupId>org.springframework.kafka</groupId>
  <artifactId>spring-kafka</artifactId>
  <version>3.1.3</version>
</dependency>
```

Or Gradle:
```groovy
implementation 'org.springframework.kafka:spring-kafka:3.1.3'
```

### Step 4: Implement Consumer Interface

The plugin generates `ISubscribeOrderCreated` interface. Implement it to handle incoming messages:

```java
package com.example.order.event.consumer;

import com.example.order.event.model.Order;
import org.springframework.stereotype.Component;

@Component
public class OrderCreatedConsumer implements ISubscribeOrderCreated {
  
  @Override
  public void subscribeOrderCreated(Order order) {
    System.out.println("Received order: " + order.getId());
    // Handle the order event
    // - Save to database
    // - Send notification
    // - Trigger business logic
  }
}
```

**Note**: The generated `StreamTopicListenerConsumer` class already creates the `Consumer<Order>` bean and wires it to your implementation. Spring Cloud Stream automatically binds it to the Kafka topic.

### Step 5: Implement Supplier Interface (for Publishing)

The plugin generates `IPublishOrderCreated` interface for publishing:

```java
package com.example.order.event.producer;

import com.example.order.event.model.Order;
import org.springframework.stereotype.Component;

@Component
public class OrderPublisher implements IPublishOrderCreated {
  
  @Override
  public Order publishOrderCreated() {
    // This method signature depends on your AsyncAPI definition
    // If publish returns void, the method would be:
    // void publishOrderCreated(Order order)
    
    Order order = Order.builder()
      .id("123")
      .amount(99.99)
      .build();
    
    // Return the order to be published
    return order;
  }
}
```

### Step 6: Configure Spring Cloud Stream and Kafka

**Application Properties** (`application.yml`):
```yaml
spring:
  kafka:
    bootstrap-servers: localhost:9092
    producer:
      key-serializer: org.apache.kafka.common.serialization.StringSerializer
      value-serializer: org.springframework.kafka.support.serializer.JsonSerializer
    consumer:
      key-deserializer: org.apache.kafka.common.serialization.StringDeserializer
      value-deserializer: org.springframework.kafka.support.serializer.JsonDeserializer
      properties:
        spring.json.type.mapping: Order:com.example.order.event.model.Order
  
  cloud:
    stream:
      default-binder: kafka
      bindings:
        subscribeOrderCreated-in-0:
          destination: orders.created
          group: order-service
        publishOrderCreated-out-0:
          destination: orders.created
```

## Publishing Messages

### Option 1: Using Publisher Interface (Supplier Pattern)

For `publish` operations with a Supplier signature:

```java
@Component
public class OrderEventService {
  
  private final IPublishOrderCreated publisher;
  
  public OrderEventService(IPublishOrderCreated publisher) {
    this.publisher = publisher;
  }
  
  public void notifyOrderCreated(String orderId, double amount) {
    Order order = Order.builder()
      .id(orderId)
      .amount(amount)
      .build();
    
    // Call the supplier method - Spring Cloud Stream handles publishing
    publisher.publishOrderCreated();
  }
}
```

### Option 2: Using StreamBridge (Direct Publishing)

For more control, use the StreamBridge pattern. Configure your AsyncAPI with StreamBridge:

```yaml
channels:
  orders.created:
    publish:
      operationId: publishOrderCreated
      message:
        $ref: '#/components/messages/OrderCreated'
```

Maven configuration:
```xml
<streamBridge>
  <ids>publishOrderCreated</ids>
  <apiPackage>com.example.order.event.producer</apiPackage>
  <modelPackage>com.example.order.event.model</modelPackage>
</streamBridge>
```

The generated `StreamBridgeProducer` class:
```java
@Configuration
public class StreamBridgeProducer {
  private StreamBridge streamBridge;

  public void publishOrderCreated(Order order) {
    streamBridge.send("orders.created", order);
  }
}
```

Use it in your service:
```java
@Service
public class OrderService {
  
  private final StreamBridgeProducer producer;
  
  public OrderService(StreamBridgeProducer producer) {
    this.producer = producer;
  }
  
  public void createOrder(CreateOrderRequest request) {
    // Create order...
    
    // Publish event
    Order order = Order.builder()
      .id(orderRecord.getId())
      .amount(request.getAmount())
      .build();
    
    producer.publishOrderCreated(order);
  }
}
```

## Comparison: Supplier vs StreamBridge

| Feature | Supplier | StreamBridge |
|---------|----------|--------------|
| Use case | Request-reply or periodic publishing | Direct control publishing |
| Binding | Spring Cloud Stream binds automatically | Manual topic specification |
| Control flow | Pull-based | Push-based |
| Configuration | Simpler | More explicit |
| Testing | Easier to mock | Requires StreamBridge mock |

**Recommendation**: Use **Supplier** (Option 1) for most use cases. It's cleaner, integrates better with Spring Cloud Stream, and is easier to test.

## Consuming Messages

Messages are automatically consumed by Spring Cloud Stream. Implement the Consumer interface as shown in Step 4.

The generated consumer class is a `@Configuration` that creates the `Consumer<Order>` bean. Spring Cloud Stream automatically:
1. Binds to the Kafka topic specified in `application.yml`
2. Deserializes JSON to Order objects
3. Passes them to your implementation

```java
@Component
public class OrderCreatedConsumer implements ISubscribeOrderCreated {
  
  private final OrderRepository orderRepository;
  
  public OrderCreatedConsumer(OrderRepository orderRepository) {
    this.orderRepository = orderRepository;
  }
  
  @Override
  public void subscribeOrderCreated(Order order) {
    // Automatically called when message arrives
    OrderEntity entity = OrderEntity.builder()
      .externalId(order.getId())
      .amount(order.getAmount())
      .build();
    
    orderRepository.save(entity);
  }
}
```

## Error Handling

### Consumer Error Handling

Add error handling in your consumer implementation:

```java
@Component
public class OrderCreatedConsumer implements ISubscribeOrderCreated {
  
  private final OrderRepository orderRepository;
  private final static Logger logger = LoggerFactory.getLogger(OrderCreatedConsumer.class);
  
  @Override
  public void subscribeOrderCreated(Order order) {
    try {
      // Process order
      OrderEntity entity = mapToEntity(order);
      orderRepository.save(entity);
    } catch (Exception e) {
      logger.error("Failed to process order: {}", order.getId(), e);
      // Publish to dead-letter topic or retry queue
      // Spring Cloud Stream supports error handling channels
      throw new RuntimeException("Order processing failed", e);
    }
  }
}
```

### Spring Cloud Stream Error Handling

Configure error handling in `application.yml`:

```yaml
spring:
  cloud:
    stream:
      bindings:
        subscribeOrderCreated-in-0:
          destination: orders.created
          group: order-service
          consumer:
            max-attempts: 3
            backoff-initial-interval: 1000
            backoff-max-interval: 10000
            backoff-multiplier: 2.0
```

## Testing

### Unit Testing Consumer

```java
@SpringBootTest
class OrderCreatedConsumerTest {
  
  @MockBean
  private OrderRepository orderRepository;
  
  @Autowired
  private OrderCreatedConsumer consumer;
  
  @Test
  void testConsumeOrderCreated() {
    Order order = Order.builder()
      .id("123")
      .amount(99.99)
      .build();
    
    consumer.subscribeOrderCreated(order);
    
    verify(orderRepository).save(argThat(entity -> 
      entity.getExternalId().equals("123") && 
      entity.getAmount() == 99.99
    ));
  }
}
```

### Integration Testing with Kafka

Use Testcontainers for integration testing:

```java
@SpringBootTest
@Testcontainers
class KafkaIntegrationTest {
  
  @Container
  static KafkaContainer kafka = new KafkaContainer(
    DockerImageName.parse("confluentinc/cp-kafka:7.5.0")
  );
  
  @Autowired
  private StreamBridgeProducer producer;
  
  @Autowired
  private OrderRepository orderRepository;
  
  @Test
  void testOrderEventFlow() throws InterruptedException {
    Order order = Order.builder()
      .id("test-123")
      .amount(50.00)
      .build();
    
    producer.publishOrderCreated(order);
    
    // Wait for async processing
    Thread.sleep(1000);
    
    OrderEntity saved = orderRepository.findByExternalId("test-123");
    assertThat(saved).isNotNull();
    assertThat(saved.getAmount()).isEqualTo(50.00);
  }
}
```

## Troubleshooting

### Messages not being consumed

1. **Check topic name**: Ensure `destination` in `application.yml` matches the AsyncAPI channel name
2. **Check consumer group**: Ensure `group` is set for consumer bindings
3. **Check bindings naming**: Spring Cloud Stream follows convention `{operationId}-in-0` for consumers, `{operationId}-out-0` for suppliers

### Deserialization errors

1. **Check JSON mapping**: Verify Jackson can deserialize your Order class
2. **Add annotations if needed**: Use `@JsonProperty` for non-standard names
3. **Configure type mapping**: Set `spring.json.type.mapping` in properties

### Connection errors

1. **Check Kafka bootstrap servers**: Verify `spring.kafka.bootstrap-servers` is accessible
2. **Check network**: Ensure Kafka is running on the specified host:port
3. **Check logs**: Look for connection timeouts or authentication errors

## Best Practices

1. **Use Supplier pattern** for cleaner code and better Spring Cloud Stream integration
2. **Implement error handling** - add try-catch and logging in consumer/supplier implementations
3. **Use interfaces** - always depend on the generated interfaces (ISubscribeX, IPublishX), not the generated classes
4. **Configure bindings explicitly** - set destination, group, and other properties in application.yml
5. **Test with containers** - use Testcontainers for realistic integration tests
6. **Monitor topics** - use kafka-console-consumer to verify messages are being published
7. **Version your events** - include version info in AsyncAPI spec for schema evolution

## Additional Resources

- [Spring Cloud Stream Documentation](https://spring.io/projects/spring-cloud-stream)
- [Spring Kafka Documentation](https://docs.spring.io/spring-kafka/docs/current/reference/html/)
- [AsyncAPI Specification](https://www.asyncapi.com/docs/specifications/v2.3.0)
- [Kafka Protocol Binding](https://github.com/asyncapi/bindings/tree/master/kafka)
