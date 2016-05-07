package io.explog.orch;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.util.Log;

import java.io.File;
import java.util.UUID;

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

    public static File mkFile(Context context, String extn) {
        File dir = context.getExternalFilesDir(null);
        if (!dir.exists()) {
            dir.mkdirs();
        }
        int numfiles = dir.listFiles().length;
        Util.logi("There are " + numfiles + " files in " + dir.toString());
        String filename = UUID.randomUUID().toString();
        if (extn != null) {
            filename += "." + extn;
        }
        return new File(dir, filename);
    }

    /* Ripped from http://stackoverflow.com/a/32856112 */
    public static boolean hasPermissionInManifest(Context context, String permissionName) {
        final String packageName = context.getPackageName();
        try {
            final PackageInfo packageInfo = context.getPackageManager()
                    .getPackageInfo(packageName, PackageManager.GET_PERMISSIONS);
            final String[] declaredPermisisons = packageInfo.requestedPermissions;
            if (declaredPermisisons != null && declaredPermisisons.length > 0) {
                for (String p : declaredPermisisons) {
                    if (p.equals(permissionName)) {
                        return true;
                    }
                }
            }
        } catch (PackageManager.NameNotFoundException e) {

        }
        return false;
    }

}
