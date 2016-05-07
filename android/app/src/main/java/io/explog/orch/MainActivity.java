package io.explog.orch;

import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.provider.MediaStore;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

import java.io.File;

public class MainActivity extends AppCompatActivity {

    RequestQueue mRequestQueue = null;
    private OrchClient mClient;
    SharedPreferences mSharedPreferences;

    static final int REQUEST_TAKE_PICTURE = 1;

    private File mCapturedImage = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
        String host = mSharedPreferences.getString("orch_host", null);
        String port = mSharedPreferences.getString("orch_port", null);

        if (port == null || port.equals("")) {
            port = "4000";
            mSharedPreferences.edit().putString("orch_port", port).commit();
        }

        if (host == null || host.equals("")) {
            Toast.makeText(this, "Just a little setup before you start drawing", Toast.LENGTH_LONG).show();
            showPrefs();
        }

        mClient = new OrchClient(this);

        mRequestQueue = Volley.newRequestQueue(this);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.item_ping: mClient.ping(); return true;

            case R.id.item_settings: showPrefs(); return true;

            case R.id.item_take_picture: takePicture(); return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void showPrefs() {
        Intent intent = new Intent(this, PreferencesActivity.class);
        startActivity(intent);
    }

    private void takePicture() {
        Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
        if (intent.resolveActivity(getPackageManager()) != null) {
            if (Util.hasPermissionInManifest(this, "android.permission.CAMERA")) {

            }
            mCapturedImage = Util.mkFile(this, "jpg");
            intent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(mCapturedImage));
            startActivityForResult(intent, REQUEST_TAKE_PICTURE);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_TAKE_PICTURE) {
            if (resultCode == RESULT_OK) {
                mClient.postFile(mCapturedImage);
            }
        }
        super.onActivityResult(requestCode, resultCode, data);
    }
}
