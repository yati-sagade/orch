package io.explog.orch;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.os.Environment;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Toast;

import com.android.volley.AuthFailureError;
import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import cz.msebera.android.httpclient.HttpEntity;
import cz.msebera.android.httpclient.HttpResponse;
import cz.msebera.android.httpclient.client.HttpClient;
import cz.msebera.android.httpclient.client.methods.HttpPost;
import cz.msebera.android.httpclient.entity.mime.HttpMultipartMode;
import cz.msebera.android.httpclient.entity.mime.MultipartEntityBuilder;
import cz.msebera.android.httpclient.entity.mime.content.FileBody;
import cz.msebera.android.httpclient.impl.client.HttpClientBuilder;


/**
 * Created by ys on 3/19/16.
 */
public class DrawingView extends View {
    private Path mDrawPath;
    private Paint mDrawPaint;
    private int mPaintColor = 0xff660000;
    private double mDensity;

    // Used to determine the width and height attributes of the resulting SVG markup
    private int maxX = 0;
    private int maxY = 0;


    // Stores the SVG path commands in sync with what mDrawPath contains.
    private StringBuilder mPathBuffer = new StringBuilder();

    public DrawingView(Context context, AttributeSet attrs) {
        super(context, attrs);
        setWillNotDraw(false);
        setup();
        mDensity = getContext().getResources().getDisplayMetrics().density;
    }

    public void undo() {
        blankOut();
    }

    public void redo() {
        Toast.makeText(getContext(), "redo is not implemented yet!", Toast.LENGTH_SHORT).show();
    }

    public void done() {
        final String path = mPathBuffer.toString().trim();
        //sendDrawingVolley(path);
        sendDrawingHttpClient(path, maxX, maxY);
        blankOut();
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        canvas.drawPath(mDrawPath, mDrawPaint);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        float touchX = event.getX(),
              touchY = event.getY();

        int   dpX = pxToDp(touchX),
              dpY = pxToDp(touchY);

        if (dpX > maxX) {
            maxX = dpX;
        }

        if (dpY > maxY) {
            maxY = dpY;
        }

        boolean resetPath = false;
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mDrawPath.moveTo(touchX, touchY);
                mPathBuffer.append(' ').append("M").append(' ').append(dpX).append(' ').append(dpY);
                break;
            case MotionEvent.ACTION_MOVE:
                mDrawPath.lineTo(touchX, touchY);
                mPathBuffer.append(' ').append("L").append(' ').append(dpX).append(' ').append(dpY);
                break;
            case MotionEvent.ACTION_UP:
                mDrawPath.lineTo(touchX, touchY);
                mPathBuffer.append(' ').append("L").append(' ').append(dpX).append(' ').append(dpY);
                resetPath = true;
                break;
            default:
                return false;
        }
        invalidate(); // onDraw() will be called immediately once we invalidate().
        return true;
    }

    private void setup() {
        mDrawPath = new Path();
        mDrawPaint = new Paint();

        mDrawPaint.setColor(mPaintColor);
        mDrawPaint.setAntiAlias(true);
        mDrawPaint.setStrokeWidth(20);
        mDrawPaint.setStyle(Paint.Style.STROKE);
        mDrawPaint.setStrokeJoin(Paint.Join.ROUND);
        mDrawPaint.setStrokeCap(Paint.Cap.ROUND);

    }

    private int pxToDp(float px) {
        return (int)((px / mDensity) + 0.5);
    }

    private void blankOut() {
        maxX = 0;
        maxY = 0;
        mDrawPath.reset();
        mPathBuffer.setLength(0);
        invalidate();
    }

    private void sendDrawingVolley(final String path) {
        RequestQueue requestQueue = Volley.newRequestQueue(getContext());
        String url = Const.getAbsoluteUrl("/insert");
        StringRequest stringRequest = new StringRequest(Request.Method.POST, url, new Response.Listener<String>() {
            @Override
            public void onResponse(String response) {
                Toast.makeText(
                        getContext(),
                        response.toString(),
                        Toast.LENGTH_LONG).show();
            }
        }, new Response.ErrorListener() {
            @Override
            public void onErrorResponse(VolleyError error) {
                Toast.makeText(
                        getContext(),
                        error.toString(),
                        Toast.LENGTH_LONG).show();
            }
        }) {
            @Override
            protected Map<String, String> getParams() throws AuthFailureError {
                Map<String, String> params = new HashMap<String, String>();
                params.put("svgpath", path);
                return params;
            }

            @Override
            public Map<String, String> getHeaders() throws AuthFailureError {
                Map<String, String> headers = new HashMap<>();
                headers.put("Content-Type", "application/x-www-form-urlencoded");
                return headers;
            }
        };
        requestQueue.add(stringRequest);
    }

    private void sendDrawingHttpClient(final String path, int width, int height) {
        File file = writeSVGToFile(path, width, height);
        if (file == null) {
            Log.e(Const.LOG_TAG, "It seems the SVG file was not written.");
            return;
        }
        HttpPost post = new HttpPost(Const.getAbsoluteUrl("/insert"));
        FileBody body = new FileBody(file);
        HttpEntity entity = MultipartEntityBuilder.create()
                .setMode(HttpMultipartMode.BROWSER_COMPATIBLE)
                .addPart("file", body)
                .build();
        post.setEntity(entity);
        HttpClient client = HttpClientBuilder.create().build();
        try {
            HttpResponse response = client.execute(post);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /// Given an SVG path, wraps it in <svg> markup, and saves it to the external storage in
    /// a randomly generated filename, and returns the File object representing the written file.
    private File writeSVGToFile(String path, int width, int height) {
        File dir = getAlbumStorageDir(getContext().getResources().getString(R.string.app_name));
        Log.d(Const.LOG_TAG, dir.toString());
        dir.mkdirs();
        String filename = UUID.randomUUID().toString() + ".svg";
        File file = new File(dir, filename);

        StringBuilder builder = new StringBuilder();

        String content =
                builder.append("<svg height=\">").append(height).append("\"")
               .append(" width=\"").append(width).append("\"")
               .append(" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">") // Opening tag completed here.
               .append(" <path d=\"").append(path).append("\" stroke=\"black\" fill=\"none\" />") // <path ... />
               .append("</svg>").toString();

        if (file.exists()) {
            file.delete();
        }

        try {
            FileOutputStream fos = new FileOutputStream(file);
            fos.write(content.getBytes("UTF-8"));
            fos.flush();
            fos.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return null;
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

        return file;
    }

    public boolean isExternalStorageWritable() {
        String state = Environment.getExternalStorageState();
        if (Environment.MEDIA_MOUNTED.equals(state)) {
            return true;
        }
        return false;
    }

    public boolean isExternalStorageReadable() {
        String state = Environment.getExternalStorageState();
        if (Environment.MEDIA_MOUNTED.equals(state) ||
            Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)) {
            return true;
        }
        return false;
    }
    public File getAlbumStorageDir(String albumName) {
        // Get the directory for the user's public pictures directory.
        if (!isExternalStorageWritable()) {
            Log.d(Const.LOG_TAG, "Storage is not writable");
        } else {
            Log.d(Const.LOG_TAG, "Storage is writable");
        }
        File file = new File(Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_PICTURES), albumName);
        if (!file.exists()) {
            if (!file.mkdirs()) {
                Log.e(Const.LOG_TAG, "Directory not created");
            } else {
                Log.d(Const.LOG_TAG, "Directory " + file.toString() + " created.");
            }
        } else {
            Log.d(Const.LOG_TAG, "Directory already exists");
        }
        return file;
    }
}
