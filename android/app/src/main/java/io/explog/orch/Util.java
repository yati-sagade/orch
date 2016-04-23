package io.explog.orch;

import android.util.Log;

/**
 * Created by ys on 4/22/16.
 */
public class Util {
    public static final String LOG_TAG = "orch";
    public static final String BASE_URL = "http://192.168.178.38:4000";

    public static final int DEFAULT_PORT = 4000;

    public static void loge(String msg) {
        Log.e(LOG_TAG, msg);
    }

    public static void logi(String msg) {
        Log.i(LOG_TAG, msg);
    }

}
