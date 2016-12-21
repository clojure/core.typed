package clojure.core.typed.compiler;

import clojure.asm.*;
import clojure.asm.commons.GeneratorAdapter;

public interface MaybePrimitiveExpr extends Expr {
  public boolean canEmitPrimitive();
  public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen);
}
