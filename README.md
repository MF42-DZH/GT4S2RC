# Spec II Cracking

Some experiments in cracking GT4 Spec II's randomizer.

Currently, this seems to work for v1.06.1. **WARNING: If inputting names into Spec II's improved keyboard, disable HD HUD / UI textures, as it renders some symbols invisible on the keyboard (e.g. what looks like `BOLT STORY` might actually be `BOLT_STORY`).**

This **does not work** for the original Randomizer Public Build from sometime 2023. **Do not use this on your own username if you would like to not be spoiled about your username's prize cars! Also do not use this to give unwanted spoilers to others' save files!**

## Usage Instructions

### Building from Scratch

This project was created using Haskell, primarily using GHC 9.10.1.
I recommend install Haskell via [`ghcup`](https://www.haskell.org/ghcup/), or if that is not an option, via Chocolatey (Windows), Homebrew (OS X) or your Linux distribution's package management system.

After you have set up Haskell (GHC 9.10.1) and Cabal 3.12.1.0 and cloned the repository, run the following commands:

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

For Windows users, pre-built releases will be provided in the Releases tab.

## Special Thanks

- Nenkai: Help with pointing in the right direction for GTHD executables to disassemble and disassembly snippets.
- TeaKanji: Providing the viability and necessity lists, as well as brute-forcing millions upon millions of usernames for their outputs.

## Credits

CARLIST, FUNCLIST and RACELIST are sourced from here https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/US_carlist.ad

Randomizer logic sourced from here: https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/PresentRoot.ad
