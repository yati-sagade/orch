package io.explog.orch;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;

/**
 * Created by ys on 3/27/16.
 */
public class DrawingFragment extends Fragment {
    public static final String ARG_TYPE = "type";

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.drawing_fragment, container, false);

        final DrawingView drawingView = (DrawingView) rootView.findViewById(R.id.drawingview);

        ImageButton undoButton = (ImageButton) rootView.findViewById(R.id.undobutton);
        undoButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                drawingView.undo();
            }
        });

        final ImageButton redoButton = (ImageButton) rootView.findViewById(R.id.redobutton);
        redoButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                drawingView.redo();
            }
        });

        final ImageButton doneButton = (ImageButton) rootView.findViewById(R.id.donebutton);
        doneButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                drawingView.done();
            }
        });

        return rootView;
    }
}
