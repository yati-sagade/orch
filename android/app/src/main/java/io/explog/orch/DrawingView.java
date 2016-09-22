package io.explog.orch;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.PointF;
import android.os.Environment;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.Toast;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;


/**
 * Created by ys on 3/19/16.
 */
public class DrawingView extends View {
    private Path mDrawPath;
    private Paint mDrawPaint;

    // TODO: Allow choosing colors.
    private int mPaintColor = 0xff000000; // This is aRGB.

    // Pixel density: Used to translate pixels to device independent units.
    private double mDensity;

    // Network IO
    private OrchClient mClient;

    // Used to determine the width and height attributes of the resulting SVG markup
    private int maxX = 0;
    private int maxY = 0;


    // Stores the SVG path commands in sync with what mDrawPath contains.
    private List<PointF> mPathFragment = new ArrayList<>();

    // Undo stack
    private Deque<List<PointF>> mUndoneFragments = new ArrayDeque<>();
    private Deque<Path>  mUndonePaths = new ArrayDeque<>();
    private Deque<List<PointF>> mPathFragments = new ArrayDeque<>();

    private Deque<Path> mPaths = new ArrayDeque<>();

    public DrawingView(Context context, AttributeSet attrs) {
        super(context, attrs);
        setWillNotDraw(false);
        setup();
        mDensity = getContext().getResources().getDisplayMetrics().density;
        mClient = new OrchClient(getContext());
    }

    public void undo() {
        if (!mPathFragments.isEmpty()) {
            mUndoneFragments.addLast(mPathFragments.removeLast());
            mUndonePaths.addLast(mPaths.removeLast());
            invalidate();
        } else {
            Toast.makeText(getContext(), "Undo not possible", Toast.LENGTH_SHORT).show();
        }
    }

    public void redo() {
        if (!mUndoneFragments.isEmpty()) {
            mPathFragments.addLast(mUndoneFragments.removeLast());
            mPaths.addLast(mUndonePaths.removeLast());
            invalidate();
        } else {
            Toast.makeText(getContext(), "Redo not possible", Toast.LENGTH_SHORT).show();
        }
    }

    public void clear() {
        mPathFragments.clear();
        mPaths.clear();
        mUndoneFragments.clear();
        mUndonePaths.clear();
        blankOut();
    }

    public void done() {
        //final String path = mPathFragment.toString().trim();
        final String path = makeSVGPathFromFragments(mPathFragments);
        sendDrawing(path, maxX, maxY);
        clear();
    }

    private String makeSVGPathFromFragments(Iterable<List<PointF>> fragments) {
        StringBuilder stringBuilder = new StringBuilder();
        maxX = 0;
        maxY = 0;
        for (Iterable<PointF> fragment : fragments) {
            boolean first = true;
            for (PointF point : fragment) {
                float touchX = point.x, touchY = point.y;
                int   dpX = pxToDp(touchX), dpY = pxToDp(touchY);

                if (dpX > maxX) {
                    maxX = dpX;
                }

                if (dpY > maxY) {
                    maxY = dpY;
                }

                // Each fragment is a set of connected line segments, but we first need to move to
                // the first point in the fragment without drawing.
                if (first) {
                    stringBuilder.append(' ').append("M").append(' ').append(dpX).append(' ').append(dpY);
                    first = false;
                } else {
                    stringBuilder.append(' ').append("L").append(' ').append(dpX).append(' ').append(dpY);
                }
            }
        }
        return stringBuilder.toString();
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        for (Path path : mPaths) {
            canvas.drawPath(path, mDrawPaint);
        }
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

        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                mUndoneFragments.clear();
                mDrawPath.moveTo(touchX, touchY);
                mPathFragment.add(new PointF(touchX, touchY));
                break;
            case MotionEvent.ACTION_MOVE:
                mDrawPath.lineTo(touchX, touchY);
                mPathFragment.add(new PointF(touchX, touchY));
                break;
            case MotionEvent.ACTION_UP:
                mDrawPath.lineTo(touchX, touchY);
                mPathFragment.add(new PointF(touchX, touchY));

                // Add fragment so it can be undone.
                mPathFragments.add(mPathFragment);
                mPathFragment = new ArrayList<>();

                // Add path to the list of paths to be drawn, and reinit the current path obj.
                mPaths.add(mDrawPath);
                mDrawPath = new Path();

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
        invalidate();
    }

    private void sendDrawing(final String path, int width, int height) {
        File file = null;
        try {
            file = writeSVGToFile(path, width, height);
        } catch (IOException ioe) {
            Toast.makeText(getContext(), ioe.getMessage(), Toast.LENGTH_SHORT).show();
        }
        if (file == null) {
            Util.loge("It seems the SVG file was not written.");
            return;
        }
        mClient.postFile(file);
    }

    /// Given an SVG path, wraps it in <svg> markup, and saves it to the external storage in
    /// a randomly generated filename, and returns the File object representing the written file.
    private File writeSVGToFile(String path, int width, int height) throws IOException {
        File file = Util.mkFile(getContext(), "svg");

        StringBuilder builder = new StringBuilder();

        String content =
                builder.append("<svg height=\"").append(height).append("\"")
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
            throw e;
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            throw e;
        } catch (IOException e) {
            e.printStackTrace();
            throw e;
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
            Util.logi("Storage is not writable");
        }
        File file = new File(Environment.getExternalStoragePublicDirectory(
                Environment.DIRECTORY_PICTURES), albumName);
        if (!file.exists() && !file.mkdirs()) {
            Util.logi("Directory not created");
        }
        return file;
    }

}
