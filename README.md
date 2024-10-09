Some experiments in cracking GT4 Spec II's randomizer.

Currently, this seems to work for v1.06.

CARLIST, FUNCLIST and RACELIST are sourced from here https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/US_carlist.ad

Randomizer logic sourced from here: https://github.com/TheAdmiester/OpenAdhoc-GT4SpecII/blob/06527e1b1cddb98ceb7f39a4df37509cc4fd9a68/src/projects/gt4o_us/gtmode/PresentRoot.ad

Tested using GHC 9.10.1, but you can probably take the code and port it to something less academic (lol).

To run with GHCi, run `ghci SpecIIData.hs`, and use `bruteForce "[USERNAME]"` to try and bruteforce a list of prize cars for randomizer.
