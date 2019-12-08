import com.google.api.client.http.HttpRequest;
import com.google.api.client.http.HttpRequestInitializer;

import java.io.IOException;

public class NopHttpInitializer implements HttpRequestInitializer {
    @Override
    public void initialize(HttpRequest httpRequest) throws IOException {
    }
}
