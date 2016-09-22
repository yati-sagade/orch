package io.explog.orch;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

/**
 * Created by ys on 3/27/16.
 */
public class DrawingFragment extends Fragment {
    public static final String ARG_TYPE = "type";
    private DrawingView mDrawingView;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setHasOptionsMenu(true);
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.drawing_fragment, container, false);

        mDrawingView = (DrawingView) rootView.findViewById(R.id.drawingview);

        ImageButton undoButton = (ImageButton) rootView.findViewById(R.id.undobutton);
        undoButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mDrawingView.undo();
            }
        });

        final ImageButton redoButton = (ImageButton) rootView.findViewById(R.id.redobutton);
        redoButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mDrawingView.redo();
            }
        });

        final ImageButton doneButton = (ImageButton) rootView.findViewById(R.id.donebutton);
        doneButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mDrawingView.done();
            }
        });

        return rootView;
    }

    @Override
    public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
        inflater.inflate(R.menu.menu_drawing, menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.item_clear: {
                if (mDrawingView != null) {
                    mDrawingView.clear();
                }
                return true;
            }
        }
        return super.onOptionsItemSelected(item);
    }
}
