Gesture test for Power Pad
==========================

![Light gray floor mat marked "Power Pad side B", with an electronics box at the top center, a red line around the electronics box warning against stepping on it, and three rows of four colored circles marked with black numbers denoting sensor positions. Sensors in top row from left to right are 1 (blue), 2 (blue), 3 (red), 4 (red). Sensors in middle row from left to right are 5 (blue), 6 (blue), 7 (red), 8 (red). Sensors in bottom row from left to right are 9 (blue), 10 (blue), 11 (red), 12 (red). White circles show presses on 6 and 7, with a Secchi-Whipple disk to show a centroid between 6 and 7 and a trail behind it showing that 9 and 10 had been pressed and released. A ticker at the bottom shows recent presses and releases, showing 9 down, 10 down, 9 up, 6 down, 10 up, 7 down.](docs/screenshot.png)

This is a demo of reading the Power Pad, a floor mat controller for
the Nintendo Entertainment System.  It is intended is a tool to
explore gestures on the Power Pad that could be useful for a game
that isn't necessarily themed around track or dance.  One hypothesis
to test is whether the Power Pad can detect the direction of a jump
before the player lands.

It displays four things:

1. A white circle on each of the 12 sensors of the Power Pad in
   controller port 2 that are currently pressed
2. If multiple sensors are pressed, a yellow [Secchi-Whipple disk]
   at the centroid (average) of their positions
3. A trail of particles that seeks the centroid, to visualize
   movement
4. A ticker showing history of recent presses and releases

If three sensors in a row or column are pressed, the centroid may
disappear behind one of them.  The trail will help you track
where it went.

[Secchi-Whipple disk]: https://en.wikipedia.org/wiki/Secchi_disk  

Credits
-------

Copyright 2022 Damian Yerrick  
License: zlib

Special thanks to supercat for the interval comparison idiom and to
whoever reverse-engineered the Power Pad for emulation
