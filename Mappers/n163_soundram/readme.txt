; n163_soundram
; tests write and read from N163 sound RAM
;
; Should print this:
;
; A7 89 30 B9 E9 A2 8B 2D ; "fibonacci" test pattern
; B8 E5 9D 82 1F A1 C0 61 ; (tests increment and wrap)
; 21 82 A3 25 C8 ED B5 A2 
; 57 F9 50 49 99 E2 7B 5D 
; D8 35 0D 42 4F 91 E0 71 
; 51 C2 13 D5 E8 BD A5 62 
; 07 69 70 D9 49 22 6B 8D 
; 38 39 3A 3B 3C 3D 3E 3F ; two rows in middle
; 40 41 42 43 44 45 46 47 ; to test non-incrementing writes
; B7 D9 90 69 F9 62 5B BD 
; 18 D5 ED C2 AF 71 20 91 
; B1 42 F3 35 28 5D 85 E2 
; 67 49 B0 F9 A9 A2 4B ED 
; 38 25 5D 82 DF 61 40 A1 
; E1 82 63 E5 48 2D 75 A2 ; not sure about the last two rows?
; 17 B9 D0 89 59 E2 3B 1D ; might be corrupted by sound unit
;
; B8 E5 9D 82 1F A1 C0 61 ; read back second line incrementally
;
; Not sure if the last two rows would be corrupted by the sound
; unit, but presumably they won't with sound disabled.
