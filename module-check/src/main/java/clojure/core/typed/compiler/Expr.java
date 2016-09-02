package clojure.core.typed.compiler;

import clojure.asm.commons.GeneratorAdapter;

public interface Expr {
	public Object eval() ;

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen);

	public boolean hasJavaClass() ;

	public Class getJavaClass() ;
}
