// copied from https://raw.githubusercontent.com/ztellman/riddley/master/src/riddley/Util.java
package clojure.core.typed.dep.riddley;

import clojure.lang.Symbol;
import clojure.lang.Compiler;

public class Util {

    public static Compiler.LocalBinding localBinding(int num, Symbol sym, Symbol tag, Object form) {
        return new Compiler.LocalBinding(num, sym, tag, Compiler.analyze(Compiler.C.EXPRESSION, form), false, null);
    }

    public static Compiler.LocalBinding localArgument(int num, Symbol sym, Symbol tag) {
        return new Compiler.LocalBinding(num, sym, tag, null, true, null);
    }
}
