package clojure.core.typed.compiler;

public interface IParser{
	Expr parse(C context, Object form) ;
}
