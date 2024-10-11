Some experiments in cracking GT4 Spec II's randomizer.

Currently, this seems to work for v1.06.

CARLIST, FUNCLIST and RACELIST are sourced from here https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/US_carlist.ad

Randomizer logic sourced from here: https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/PresentRoot.ad

Tested using GHC 9.10.1, but you can probably take the code and port it to something less academic (lol).

To run the program, execute `runghc SpecIIData.hs`, and input a username to try and bruteforce a list of prize cars for randomizer. You can also compile it into a program with GHC, using `ghc -main-is SpecIIData SpecIIData.hs`, then run it as you would a normal executable.

To use `ViabilityTester.hs` effectively, it must be compiled with the threaded option and optimisations. I recommend the command `ghc -main-is ViabilityTester ViabilityTester.hs -O2 -threaded` and then enabling the RTS system when running by running the compiled output as `ViabilityTester[.exe] +RTS -N6`.

Car viability metric list provided by TeaKanji. If any of the four primary lists (`CARLIST.txt`, `RACELIST.txt`, `FUNCLIST.txt`, `VIABILITY.txt`) are modified, please run `runghc GenerateCombinedList.hs` to initialise the new data into `COMBINEDLIST.txt`.
