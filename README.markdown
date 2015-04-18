# hesh - Haskell Extensible Shell

hesh makes writing scripts in Haskell easier. It automates a set of shortcuts that make writing scripts in Haskell more convenient.

hesh does not have an interactive mode; it's intended for scripts. hesh is not an interpreter or compiler itself; instead it transforms your single-file script into a complete Haskell program and then compiles it to a native binary.

## Automatic Cabal File Generation

hesh compiles your program with Cabal. To do that it creates a .cabal file for you. For example, the simple hesh script:

```
main = return ()
```

will get a .cabal file that looks something like:

```
-- This Cabal file generated using the Cartel library.
-- Cartel is available at:
-- http://www.github.com/massysett/cartel
--
-- Generated on: 2015-02-08 01:24:48.387367 PST
-- Cartel library version: 0
name: script
version: 0.1
cabal-version: >= 1.18
build-type: Simple
license: AllRightsReserved
license-file: LICENSE
category: shell

Executable script
  main-is: Main.hs
  default-language: Haskell2010
  build-depends:
  base
```

In this simple case a .cabal file is unnecessary as you could just run the Haskell script with `runhaskell`. However, the .cabal file will be useful for scripts with library dependencies, as we'll see next.

## Automatic Import Package Lookups

hesh parses your script for module imports, looks up suitable packages from the Hackage database, and adds them to the .cabal file for you. If your script was:

```
import Prelude (return)

main = return ()
```

hesh would find that `Prelude` is a member of `base` and add `base` as a `build-depends` to the .cabal file for you. However, hesh will always include `base`, so a more interesting example is:

```
import qualified Data.Text as Text
import Data.Text.IO (putStrLn)
import Prelude () -- To hide Prelude.putStrLn

main = putStrLn (Text.pack "Hello, text!")
```

hesh will automatically discover that it needs to add the `text` package to the .cabal file in this case.

### Hackage Lookups

To do these lookups, hesh reads the Cabal package list (usually in `~/.cabal/packages/hackage.haskell.org/00-index.tar.gz`) which is maintained by the `cabal` program. This means that the packages it chooses will be based on the last time that you ran `cabal update`.

Note that:

* The first time hesh runs it needs to build and cache a more efficient representation of the hackage list, which can take up to a few minutes.
* Unfortunately you cannot currently use separate package databases for each script/Cabal sandbox.

## Automatic Module Imports

Sometimes you only want to use a single function from a module, especially when first drafting a script. It's distracting to have to add the module to the imports list. hesh lets you write the fully qualified name inline and adds the module import for you (adding the appropriate Hackage package as well, if necessary). We can rewrite the previous example as:

```
main = Data.Text.IO.putStrLn (Data.Text.pack "Hello, text!")
```

## Using a Shebang

hesh can be used to create a self-contained script, as with `runghc` or `runhaskell`. Take the following script:

```
#!/usr/bin/env hesh

main = putStrLn . show =<< System.Environment.getArgs
```

If you saved the above into a script named `showargs.hesh` and marked it as executable (`chmod +x`), you could use it like:

```
$ ./showargs.hesh
[]
$ ./showargs.hesh 1 2 3
["1","2","3"]
```

## Implicit main

We could further simplify the previous example by leaving off the
`main = ` since hesh will assume that the final lines of the script are
your main function.

```
#!/usr/bin/env hesh

putStrLn . show =<< System.Environment.getArgs
```

This might seem insignificant, but becomes more convenient with multiple lines:

```
#!/usr/bin/env hesh

progName <- System.Environment.getProgName
args <- System.Environment.getArgs
putStrLn $ progName ++ ": " ++ Data.List.intercalate " " args
```

compared to:

```
#!/usr/bin/env hesh

main = do
  progName <- System.Environment.getProgName
  args <- System.Environment.getArgs
  putStrLn $ progName ++ ": " ++ Data.List.intercalate " " args
```

## Syntax

`$[]` creates a process.
$[command-name arg1 arg2 ...]
all arguments are optional
unquoted arguments are subject to shell expansion (currently just filename expansion)

## Notes

For simplicity, hesh creates a new (temporary) directory for every script (and every change to your script). This keeps it from messing with your global/user Cabal installed packages and provides some determinism. However, it means that all Hackage packages are rebuild every time you change the script. This is by far the slowest part of running hesh on a new script or changed script. It can also lead to filling up your temporary directory quickly while developing and testing a script.
