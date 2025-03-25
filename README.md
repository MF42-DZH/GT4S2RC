# Spec II RNG Cracking

Some experiments in cracking GT4 Spec II's randomizer.

Currently, this seems to work for v1.09. **WARNING: If inputting names into Spec II's improved keyboard, disable HD HUD / UI textures, as it renders some symbols invisible on the keyboard (e.g. what looks like `BOLT STORY` might actually be `BOLT_STORY`).**

This **does not work** for the original Randomizer Public Build from sometime 2023. **Do not use this on your own username if you would like to not be spoiled about your username's prize cars! Also do not use this to give unwanted spoilers to others' save files!**

## Usage Instructions

### Using a Pre-built Release

For Windows users, pre-built releases will be provided in the Releases tab.
You do not need to install any extra components in order to run it.
Ensure you are using some kind of command shell (CMD, PS, PWSH, etc.) and your current directory is in the directory where you have unzipped all the contents of the ZIP file to.

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

You should now have `SpecII-Seed-Analyzer`, `SpecII-Viability-Tester`, `SpecII-Permutation-Tester`, `SpecII-Generate-Combined-Lists`, `SpecII-Unviability-Tester`, `SpecII-List-Analyzer`, `SpecII-Bitflip-Tester`, `SpecII-Stdin-Analyzer`, `SpecII-100-Tester-Stdin`, `SpecII-100-Existence-Tester`, `SpecII-Hash-Breaker` in your PATH.

Before anything else, while still in the repository, run `SpecII-Generate-Combined-Lists` in order to (re-)generate the combined lists and necessity list you see at the top level of the repository with up-to-date data.
You will need to have these files in the current working directory in order to use the functionality of the programs built.

## Programs

- `SpecII-Seed-Analyzer`: For analyzing the prizes of a single username in detail. Specify `--hash` to search by username hash instead. **This is probably what you want.**
- `SpecII-Viability-Tester`: This runs through all possible usernames of a length less than or equal to a length specified by the user and checks which username is the 'best'.
- `SpecII-Unviability-Tester`: Similar to `SpecII-Viability-Tester`, but tries to find the worst username instead.
- `SpecII-Permutation-Tester`: This runs through all possible capitalizations of a username input by a user and checks which username is the 'best'.
- `SpecII-Bitflip-Tester`: Similar to `SpecII-Permutation-Tester`, but also tries flipping the least significant bit of each character in the username.
- `SpecII-List-Analyzer`: This runs through all the usernames in a file, whose path is specified by the user. Separate each username into its own line in the file. It will also try to find the 'best' username out of all of them.
- `SpecII-Stdin-Analyzer`: Similar to `SpecII-List-Analyzer`, but will take usernames via standard input. This would be typically used with external programs that can generate hundreds of thousands or even millions of usernames on separate lines, whose output is piped into this analyzer (e.g. `../some-username-generator | SpecII-Stdin-Analyzer`). Specify `--missing` to divide the viability of each username by the number of missing cars for a 100% prize car only playthrough.
- `SpecII-Generate-Combined-Lists`: This is a utility command used for regenerating the combined lists and necessity lists at the top level of the file structure of these programs.
- `SpecII-100-Tester-Stdin`: Similar to `SpecII-Stdin-Analyzer`, but hyper-optimized for searching for usernames with the least amount of missing cars for a 100% prize-cars only playthrough.
- `SpecII-100-Existence-Tester`: Tests to see if there even is a username hash that can get a 100% prize cars only save.
- `SpecII-Hash-Breaker`: Tries to find a valid username that matches a username hash. **Do not ask this program to look for usernames longer than 5 characters long unless you have at least 32 GB of RAM, this program eats memory for breakfast!**

Where 'best username' is referenced, it means it has the highest average viability of all the cars. Some programs will also let you divide this viability by the number of missing cars, to let you find usernames most viable for getting close to a 100% run on only prize cars.

## Special Thanks

- Nenkai: Help with pointing in the right direction for GTHD executables to disassemble and disassembly snippets.
- TeaKanji: Providing the viability and necessity lists, as well as brute-forcing millions upon millions of usernames for their outputs.

## Credits

CARLIST, FUNCLIST and RACELIST are sourced from here https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/master/src/projects/gt4o_us/gtmode/US_carlist.ad

Randomizer logic sourced from here: https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/master/src/projects/gt4o_us/gtmode/PresentRoot.ad
