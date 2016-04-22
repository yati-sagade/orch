package io.explog.orch;

import android.content.Context;

import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.AsyncHttpResponseHandler;
import com.loopj.android.http.RequestParams;

import cz.msebera.android.httpclient.HttpEntity;

/**
 * Created by ys on 3/20/16.
 */
public class Client {
    private static final String BASE_URL = "http://192.168.178.38:4000";

    private static final String USER_AGENT = "Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:45.0)";

    private static AsyncHttpClient client;

    static {
        client = new AsyncHttpClient();
        client.setUserAgent(USER_AGENT);
    }

    public static void get(String url, RequestParams params, AsyncHttpResponseHandler responseHandler) {
        client.get(getAbsoluteUrl(url), params, responseHandler);
    }

    public static void post(String url, RequestParams params, AsyncHttpResponseHandler responseHandler) {
        client.post(getAbsoluteUrl(url), params, responseHandler);
    }

    public static void post(Context context, String url, HttpEntity entity, String contentType, AsyncHttpResponseHandler responseHandler) {
        client.post(context, getAbsoluteUrl(url), entity, contentType, responseHandler);
    }

    public static String getAbsoluteUrl(String relativeUrl) {
        if (relativeUrl.length() == 0) {
            return BASE_URL;
        }
        String prefix = (relativeUrl.charAt(0) == '/') ? "" : "/";
        return BASE_URL + prefix + relativeUrl;
    }
}
