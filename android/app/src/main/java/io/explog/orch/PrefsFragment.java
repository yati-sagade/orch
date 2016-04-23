package io.explog.orch;

import android.os.Bundle;
import android.preference.PreferenceFragment;

/**
 * Created by ys on 4/23/16.
 */
public class PrefsFragment extends PreferenceFragment {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);
    }
}
