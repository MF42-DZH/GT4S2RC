# Spec II Cracking

Some experiments in cracking GT4 Spec II's randomizer.

Currently, this seems to work for v1.06.1.

## Usage Instructions

### Building from Scratch

This project was created using Haskell, primarily using GHC 9.10.1.
I recommend install Haskell via [`ghcup`](https://www.haskell.org/ghcup/), or if that is not an option, via Chocolatey (Windows), Homebrew (OS X) or your Linux distribution's package management system.

After you have set up Haskell and Cabal and cloned the repository, run the following commands:

```
git clone https://github.com/MF42-DZH/GT4S2RC.git
cd GT4S2RC
cabal build
cabal install --overwrite-policy=always
```

You should now have `SpecII-Seed-Analyzer`, `SpecII-Viability-Tester`, `SpecII-Permutation-Tester`, `SpecII-Generate-Combined-Lists`, `SpecII-Unviability-Tester` in your PATH.

Before anything else, while still in the repository, run `SpecII-Generate-Combined-Lists` in order to (re-)generate the combined lists and necessity list you see at the top level of the repository with up-to-date data.
You will need to have these files in the current working directory in order to use the functionality of the programs built.

### Using a Pre-built Release

**Coming Soon!**

## Special Thanks

- Nenkai: Help with pointing in the right direction for GTHD executables to disassemble and disassembly snippets.
- TeaKanji: Providing the viability and necessity lists, as well as brute-forcing millions upon millions of usernames for their outputs.

## Credits

CARLIST, FUNCLIST and RACELIST are sourced from here https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/US_carlist.ad

Randomizer logic sourced from here: https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/PresentRoot.ad



Tested using GHC 9.10.1, but you can probably take the code and port it to something less academic (lol).

To run the program, execute `runghc SpecIIData.hs`, and input a username to try and bruteforce a list of prize cars for randomizer. You can also compile it into a program with GHC, using `ghc -main-is SpecIIData SpecIIData.hs`, then run it as you would a normal executable.

To use `ViabilityTester.hs` effectively, it must be compiled with the threaded option and optimisations. I recommend the command `ghc -main-is ViabilityTester ViabilityTester.hs -O2 -threaded` and then enabling the RTS system when running by running the compiled output as `ViabilityTester[.exe] +RTS -N6`. You can use `UnviabilityTester.hs` in the same way, but it instead finds the worst username of a maximum length.

Car viability metric list provided by TeaKanji. If any of the five primary lists (`CARLIST.txt`, `RACELIST.txt`, `FUNCLIST.txt`, `VIABILITY.txt`, `RACENAMES.txt`) are modified, please run `runghc GenerateCombinedList.hs` to initialise the new data into `COMBINEDLIST.txt`.
