* Do we need to access the global env?
* How to represent types
** defs
*** would like to use names like float, namespace
*** could hard-code un-namespaced types
**** must be a better way
*** create a new environment based on types
**** only types are allowed
**** by default, imports typed.core instead of clojure.core
**** the good:
***** everything is namespaced
***** can use any names we wish
***** provide a separate ns macro to fully configure namespacing
**** the bad:
***** redundant namespace macro (?)
* need to pass "env" around, like CLJS, for symbol resolution
** type env should be accumulated in my own namespace
** don't rely on runtime environment
