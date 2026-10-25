# HORI Tracker

by TakuikaNinja

This [mapper 218](https://www.nesdev.org/wiki/INES_Mapper_218) program displays the trackball and button state for the Famicom [HORI Track](https://www.nesdev.org/wiki/Hori_Track) controller.

## Building

The CC65 toolchain is required to build the program: https://cc65.github.io/
- A simple `make` should then work.
- Optionally, install Go to run `vramstruct.go` used for compressing the CHR data.

## Screenshot

![Example screenshot with input display](/horitracker.png)

## Acknowledgements

- `chars.chr` was converted and modified from the following placeholder CHR sheet: https://www.nesdev.org/wiki/File:Jroatch-chr-sheet.chr.png
  - It originally contained tiles from Generitiles by Drag, Cavewoman by Sik, and Chase by shiru.
- [MesenCE](https://github.com/nesdev-org/MesenCE) was used for testing, as I do not own the device.
- The NESdev Wiki, Forums, and Discord have been a massive help. Kudos to everyone keeping this console generation alive!
