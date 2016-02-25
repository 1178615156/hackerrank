import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Arrays;
import java.util.List;

/**
 * Created by yuJieShui on 2016/2/21.
 */
interface Json {
    public static <T> String toJson(T t) {
        return t.toString();
    }
}

public interface H {
    public default int say(Integer integer) {
        return this.bay() + integer;
    }

    public default Class<? extends H> clazz() {
        return this.getClass();
    }

    public default int bay() {
        return 1;
    }


    public static void main(String[] s) throws ScriptException, FileNotFoundException {

        List<String> list = Arrays.asList("aa", "bb", "cc");
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("JavaScript");
        engine.getContext();
        File hello = new File("./hello");
        System.out.println(hello.getParent());
        engine.eval(new FileReader("./js.js"));
        engine.eval("var s = 1");
        engine.eval("s += 1");
        engine.eval("print");
    }
}
