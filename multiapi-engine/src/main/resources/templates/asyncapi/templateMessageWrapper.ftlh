package ${wrapperPackage};

public class MessageWrapper<T, Y> {

  private T payload;

  private Y key;

  public MessageWrapper(final Y key, final T payload) {
    this.payload = payload;
    this.key = key;
  }

  public T getPayload() {
   return payload;
  }

  public void setPayload(T payload) {
    this.payload = payload;
  }

  public Y getKey() {
    return key;
  }

  public void setKey(Y key) {
    this.key = key;
  }

}