package io.explog.orch;

/**
 * Created by ys on 4/21/16.
 */
public class Const {
    public static final String BASE_URL = "http://192.168.178.38:4000";

    public static final String LOG_TAG = "orch";

    public static String getAbsoluteUrl(String relativeUrl) {
        if (relativeUrl.length() == 0) {
            return BASE_URL;
        }
        String prefix = (relativeUrl.charAt(0) == '/') ? "" : "/";
        return BASE_URL + prefix + relativeUrl;
    }
}
