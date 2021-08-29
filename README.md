# supercell

## Tips and Tricks and how to compile our code

To compile the Haskell code, just run `stack build`.
Then open `./gui/supercell-gui/CMakeLists.txt` in Qt Creator.
You should then be able to build and run the GUI like any other Qt program.
You can also run the interactive interpreter on the Haskell code alone by running `stack ghci supercell:lib`.

### Troubleshooting compiler errors

When building Supercell for the first time you might see the following error:

```
--  While building package regex-posix-0.96.0.0 (scroll up to its section to see the error) using:
      C:\sr\setup-exe-cache\x86_64-windows\Cabal-simple_Z6RU0evB_3.2.1.0_ghc-8.10.4.exe --builddir=.stack-work\dist\274b403a build --ghc-options " -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
```

This can be fixed by running the following commands, exactly as written and in order:

```
stack --resolver lts-18.1 exec -- ghc-pkg unregister regex-posix-0.96.0.0 --force
stack build regex-posix-0.96.0.0 --flag regex-posix:_regex-posix-clib --resolver lts-18.1
```

(The first command can sometimes give an error. This error is safe to ignore; simply continue on with the second command.)

After running the two commands above, commands such as `stack build` or `stack run` should complete successfully and without error.

## How to run the UI

Here are the fields in the GUI:

| Field name | Contents |
| --- | --- |
| Name | Name of variable |
| Type | Type of variable (e.g. `TNum`, `TBool`, `TList(something)`) |
| Expr | eg. `1+1` or `Mean([1,5,10])` or `If(1=1,10,20)` |
| Value | Result returned |
