# core.typed

<a href='http://typedclojure.org'><img src='images/part-of-typed-clojure-project.png'></a>

Optional typing in Clojure, as a library.

## Releases and Dependency Information

Latest stable release is 0.3.0.

Leiningen dependency information:

```clojure
[org.clojure/core.typed "0.3.0"]

...
; for very recent releases
:repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

; for slim jars, follow version string with: :classifier "slim"
```

Maven dependency information:

```XML
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.typed</artifactId>
  <version>0.3.0</version>
  <!-- slim jar -->
  <!-- <classifier>slim</classifier> -->
</dependency>
```

The default jars contain AOT files for faster loading. If jar size is a concern, consider
using the slim jar in production.

## Compatibility

`core.typed` supports Clojure 1.6.0 and JDK 1.7+.

## [Talk] Clojure Conj 2012

[Video](http://www.youtube.com/watch?v=wNhK8t3uLJU)

## Mailing List and IRC

Use the core.typed [mailing list](https://groups.google.com/forum/?fromgroups#!forum/clojure-core-typed) for core.typed discussion, 
or try #typed-clojure on Freenode (the main developer is ambrosebs).

[IRC Logs](http://logs.lazybot.org/irc.freenode.net/%23typed-clojure) (Thanks to [Anthony Grimes](https://twitter.com/IORayne)!)

## Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

[API Reference](http://clojure.github.io/core.typed/)

[Ambrose's blog](http://frenchy64.github.io/)

## Leiningen Plugin

[lein-typed](https://github.com/frenchy64/lein-typed)

## Vim Plugin

[vim-typedclojure](https://github.com/typedclojure/vim-typedclojure)

## Quickstart

`(clojure.core.typed/ann v t)` gives var `v` the static type `t`.

`(clojure.core.typed/ann-form f t)` ensures form `f` is of the static type `t`.

`(clojure.core.typed/check-ns)` type checks the current namespace.

`(clojure.core.typed/cf t)` type checks the form `t`.

## Examples

[core.async Rock-paper-scissors](https://github.com/clojure/core.typed/blob/master/module-check/src/test/clojure/clojure/core/typed/test/rps_async.clj)

## Developer Information

- [Typed Clojure Contributor Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing guidelines](CONTRIBUTING.md)
- [GitHub Project](https://github.com/clojure/core.typed)
- [Bug Tracker](http://dev.clojure.org/jira/browse/CTYP)
- [Continuous Integration](http://build.clojure.org/job/core.typed/)
- [Compatibility Test Matrix](http://build.clojure.org/job/core.typed-test-matrix/)

<!---
## Future work

* Equality filters for occurrence typing
* Unify AST with ClojureScript
* Namespace dependency management
* Track changes to Typed Racket
  * https://github.com/plt/racket/compare/6105ce8b2087...71d6189132ce
-->

## Contributors

Thanks to the following people for contributing to core.typed:

* Stephen Compall (S11001001)
* Andrew Brehaut (brehaut)
* Christopher Spencer (cspencer)
* Reid McKenzie (arrdem)
* Di Xu (xudifsd)
* Nicola Mometto (Bronsa)
* Chas Emerick (cemerick)
* Jake Goulding (shepmaster)
* Andy Fingerhut
* Aravind K N (arav93)
* Allen Rohner (arohner)
* Minori Yamashita (ympbyc)
* Kyle Kingsbury (aphyr)
* Nathan Sorenson

## Sponsors

Development is sponsored (via [crowdfunding](http://www.indiegogo.com/projects/typed-clojure)) by

<div>
  <div>
    <a href="http://brickalloy.com/">
      <img src="http://typedclojure.org/images/sponsors/brick_alloy_2_37.png" alt="Brick Alloy">
    </a>
  </div>
  <div class="col-md-2">
    <a href="http://cognitect.com/">
      <img src="http://typedclojure.org/images/sponsors/cognitect_black_1_27.png" alt="Cognitect">
    </a>
  </div>
  <div>
    <a href="http://www.circleci.com/">
      <img src="http://typedclojure.org/images/sponsors/circleci_logoweb.jpg" alt="CircleCI">
    </a>
  </div>
  <div class="col-md-2">
    <a href="https://www.hackerschool.com/">
      <img src="http://typedclojure.org/images/sponsors/hackerschool.png" alt="Hacker School">
    </a>
  </div>
</div>
<div>
  <div>
    <a href="http://snowplowanalytics.com/">
      <img src="http://typedclojure.org/images/sponsors/snowplow-logo.png" alt="Snowplow Analytics">
    </a>
  </div>
  <div>
    <a href="http://leonidasoy.fi/">
      <img src="http://typedclojure.org/images/sponsors/leonidas.png" alt="Leonidas">
    </a>
  </div>
  <div>
    <a href="http://getprismatic.com/">
      <img src="http://typedclojure.org/images/sponsors/prismatic-logo.png" alt="Prismatic">
    </a>
  </div>
</div>
<div>
  <div style="background-color: #555;">
    <a href="http://www.thortech-solutions.com/">
      <img src="http://typedclojure.org/images/sponsors/thortech.png" alt="ThorTech Solutions">
    </a>
  </div>
  <div>
    <a href="http://sonian.com/">
      <img src="http://typedclojure.org/images/sponsors/sonian.png" alt="Sonian">
    </a>
  </div>
  <div>
    <a href="https://twitter.com/srseverance">Shannon Severance</a>
  </div>
</div>
<div>
  <div>
    <a href="http://cursiveclojure.com/">
      <img src="http://typedclojure.org/images/sponsors/cursive.png" alt="Cursive Clojure">
    </a>
  </div>
</div>
</div>


## YourKit

YourKit is kindly supporting core.typed and other open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).

### tools.analyzer

Copyright © 2013-2015 Nicola Mometto, Rich Hickey & contributors.

Distributed under the Eclipse Public License, the same as Clojure.

### tools.analyzer.jvm

Copyright © 2013-2015 Nicola Mometto, Rich Hickey & contributors.

Distributed under the Eclipse Public License, the same as Clojure.

### jvm.tools.analyzer

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).

### data.priority-map

Copyright (C) 2013 Mark Engelberg

Distributed under the Eclipse Public License, the same as Clojure.

### math.combinatorics

Distributed under the Eclipse Public License, the same as Clojure.

### tools.namespace

Copyright © 2012 Stuart Sierra All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 1.0 which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.

### tools.reader

Copyright © 2013-2015 Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL. (See the file epl.html.)

### core.contracts

Copyright (c) Rich Hickey, Michael Fogus and contributors, 2012. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound bythe terms of this license. You must not remove this notice, or any other, from this software.

### core.unify

Copyright (c) Rich Hickey and Michael Fogus, 2012. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound bythe terms of this license. You must not remove this notice, or any other, from this software.

### core.cache

Copyright (c) Rich Hickey, Michael Fogus and contributors, 2012. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound bythe terms of this license. You must not remove this notice, or any other, from this software.

### core.memoize

Copyright (c) Rich Hickey and Michael Fogus, 2012, 2013. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound bythe terms of this license. You must not remove this notice, or any other, from this software.

### ASM

Copyright (c) 2000-2011 INRIA, France Telecom
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holders nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
THE POSSIBILITY OF SUCH DAMAGE.
