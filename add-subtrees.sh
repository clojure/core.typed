#!/bin/bash

set -e

git subtree add -P module-analyzer.jvm https://github.com/clojure/core.typed.analyzer.jvm.git 31892d2a3e1083d0cd495c39e4704eacf14e0d96
git subtree add -P module-annotator.jvm https://github.com/clojure/core.typed.annotator.jvm.git 25d2d433c983b16445f8da9d9cc80789e9673bdf
git subtree add -P module-checker.jvm https://github.com/clojure/core.typed.checker.jvm.git 33f3e269a1de640543a6c64fc33fa6faaf071e7a
git subtree add -P module-runtime.jvm https://github.com/clojure/core.typed.runtime.jvm.git c0a07a2dd2d1d67196f38b517c47905531179842
git subtree add -P module-lang.jvm https://github.com/typedclojure/core.typed.lang.jvm.git da7a11930fbbc237376cd14550231c90cab05146
git subtree add -P module-lib.clojure https://github.com/typedclojure/core.typed.lib.clojure.git b090c2f3d94714e8d248aad0b3e1b2305dd58eb1
