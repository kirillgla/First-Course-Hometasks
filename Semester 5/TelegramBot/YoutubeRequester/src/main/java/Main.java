import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.VideoSnippet;

import java.io.IOException;

public class Main {
    private static final String APPLICATION_NAME = "API code samples";
    private static final JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();
    private static final NetHttpTransport HTTP_TRANSPORT = new NetHttpTransport();
    public static final long RESULT_LIMIT = 4;

    public static void main(String[] args) throws IOException {
        YouTube youTube = new YouTube
                .Builder(HTTP_TRANSPORT, JSON_FACTORY, new NopHttpInitializer())
                .setApplicationName(APPLICATION_NAME)
                .build();
        YouTube.Videos.List request = youTube
                .videos()
                .list("snippet,statistics")
                .setKey("AIzaSyABRYd1lek4Jjx7XhoxItXMGAMJBFNJIKE")
                .setMaxResults(RESULT_LIMIT)
                .setChart("mostPopular")
                .setRegionCode("US");
        System.out.println("Top " + RESULT_LIMIT + " trending videos:");
        request.execute().getItems().forEach(video -> {
            VideoSnippet snippet = video.getSnippet();
            System.out.println();
            System.out.print("Title: ");
            System.out.println(snippet.getTitle());
            System.out.print("Channel: ");
            System.out.println(snippet.getChannelTitle());
        });
    }
}