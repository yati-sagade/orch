package io.explog.orch;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.preference.PreferenceManager;
import android.widget.Toast;

import com.loopj.android.http.HttpGet;

import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import cz.msebera.android.httpclient.HttpEntity;
import cz.msebera.android.httpclient.HttpResponse;
import cz.msebera.android.httpclient.client.HttpClient;
import cz.msebera.android.httpclient.client.methods.HttpPost;
import cz.msebera.android.httpclient.entity.mime.HttpMultipartMode;
import cz.msebera.android.httpclient.entity.mime.MultipartEntityBuilder;
import cz.msebera.android.httpclient.entity.mime.content.FileBody;
import cz.msebera.android.httpclient.impl.client.HttpClientBuilder;

/**
 * Created by ys on 4/23/16.
 */
public class OrchClient {
    private Context mContext;
    private SharedPreferences mSharedPreferences;

    public OrchClient(Context context) {
        mContext = context;
        mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
    }

    public void postFile(File file) {
        new PostFileTask().execute(file);
    }

    public void ping() {
        new PingTask().execute();
    }

    private class PingTask extends AsyncTask<Void, Void, Result> {
        @Override
        protected void onPostExecute(Result result) {
            if (result == null) {
                Toast.makeText(mContext, "Could not ping server, please check your settings", Toast.LENGTH_LONG);
                return;
            }
            String content = result.content;
            int statusCode = result.statusCode;
            int toastLength = Toast.LENGTH_SHORT;
            if (statusCode / 100 != 2) {
                Util.loge("Status code " + statusCode + " returned from the server.");
                toastLength = Toast.LENGTH_LONG;
            }
            Toast.makeText(mContext, content, toastLength).show();
        }

        @Override
        protected Result doInBackground(Void... params) {
            HttpClient client = HttpClientBuilder.create().build();
            String url = getAbsoluteUrl("/ping");
            if (url == null) {
                return null;
            }
            HttpGet get = new HttpGet(url);
            try {
                HttpResponse response = client.execute(get);
                InputStream istream = response.getEntity().getContent();
                int statusCode = response.getStatusLine().getStatusCode();
                String content = IOUtils.toString(istream, "utf8");
                return new Result(content, statusCode);
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
            return null;
        }
    }

    private class PostFileTask extends AsyncTask<File, Void, Result> {

        @Override
        protected void onPostExecute(Result response) {
            if (response == null) {
                Toast.makeText(mContext, "Could not send drawing, please check your settings", Toast.LENGTH_LONG);
                return;
            }
            String content = response.content;
            int statusCode = response.statusCode;
            int toastLength = Toast.LENGTH_SHORT;
            if (statusCode / 100 != 2) {
                Util.loge("Status code " + statusCode + " returned from the server.");
                toastLength = Toast.LENGTH_LONG;
            }
            Toast.makeText(mContext, content, toastLength).show();
        }

        @Override
        protected Result doInBackground(File... params) {
            if (params.length == 0) {
                Util.logi("PostFileTask::doInBackground() called with no files.");
                return null;
            }
            String url = getAbsoluteUrl("/insert");
            if (url == null) {
                return null;
            }
            File file = params[0];
            HttpPost post = new HttpPost(url);
            FileBody body = new FileBody(file);
            HttpEntity entity = MultipartEntityBuilder.create()
                    .setMode(HttpMultipartMode.BROWSER_COMPATIBLE)
                    .addPart("file", body)
                    .build();
            post.setEntity(entity);
            HttpClient client = HttpClientBuilder.create().build();
            try {
                HttpResponse response = client.execute(post);
                InputStream istream = response.getEntity().getContent();
                int statusCode = response.getStatusLine().getStatusCode();
                String content = IOUtils.toString(istream, "utf8");
                file.delete();
                return new Result(content, statusCode);
            } catch (IOException e) {
                e.printStackTrace();
            }
            return null;
        }
    }

    private class Result {
        public final String content;
        public final int statusCode;

        public Result(String content, int statusCode) {
            this.content = content;
            this.statusCode = statusCode;
        }
    }

    public String getAbsoluteUrl(String relativeUrl) {
        String baseUrl = getBaseUrl();
        if (baseUrl == null) {
            return null;
        }
        if (relativeUrl.length() == 0) {
            return baseUrl;
        }
        String prefix = (relativeUrl.charAt(0) == '/') ? "" : "/";
        return baseUrl + prefix + relativeUrl;
    }

    public String getBaseUrl() {
        String host = mSharedPreferences.getString("orch_host", null);
        if (host == null || host.equals("")) {
            Toast.makeText(mContext, "Please add the host in settings", Toast.LENGTH_SHORT);
            return null;
        }
        String port = mSharedPreferences.getString("orch_port", String.valueOf(Util.DEFAULT_PORT));
        return new StringBuilder("http://").append(host).append(":").append(port).toString();
    }
}
