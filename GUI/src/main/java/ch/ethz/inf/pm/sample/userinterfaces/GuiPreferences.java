package ch.ethz.inf.pm.sample.userinterfaces;

import java.util.prefs.Preferences;

/**
 * Lucas Brutschy
 * Date: 10/26/12
 * Time: 3:38 PM
 */
public class GuiPreferences {

    private static final String NODE = "ch/ethz/inf/pm/sample";
    private static final int VERSION = 1;

    // Field Labels
    private static final String LABEL_VERSION = "version";
    private static final String LABEL_file = "file";
    private static final String LABEL_method = "method";
    private static final String LABEL_compiler = "compiler";
    private static final String LABEL_analysis = "analysis";
    private static final String LABEL_heapAnalysis = "heapAnalysis";

    // Default Values
    private static final String DEF_file = "";
    private static final String DEF_method = "";
    private static final int DEF_compiler = 0;
    private static final int DEF_analysis = 0;
    private static final int DEF_heapAnalysis = 0;

    // Fields
    public String file = DEF_file;
    public String method = DEF_method;
    public int compiler = DEF_compiler;
    public int analysis = DEF_analysis;
    public int heapAnalysis = DEF_heapAnalysis;


    public void putSettings() {
        Preferences preferences = Preferences.userRoot().node(NODE);

        preferences.putInt(LABEL_VERSION, VERSION);
        preferences.put(LABEL_file,file);
        preferences.put(LABEL_method,method);
        preferences.putInt(LABEL_compiler,compiler);
        preferences.putInt(LABEL_analysis,analysis);
        preferences.putInt(LABEL_heapAnalysis,heapAnalysis);
    }

    public static GuiPreferences getSettings() {
        Preferences preferences = Preferences.userRoot().node(NODE);

        int version = preferences.getInt(LABEL_VERSION,VERSION);
        if (version != VERSION) {
            new GuiPreferences().putSettings();
        }

        GuiPreferences local = new GuiPreferences();
        local.file = preferences.get(LABEL_file,DEF_file);
        local.method = preferences.get(LABEL_method,DEF_method);
        local.compiler = preferences.getInt(LABEL_compiler,DEF_compiler);
        local.analysis = preferences.getInt(LABEL_analysis,DEF_analysis);
        local.heapAnalysis = preferences.getInt(LABEL_heapAnalysis,DEF_heapAnalysis);

        return local;
    }

}
