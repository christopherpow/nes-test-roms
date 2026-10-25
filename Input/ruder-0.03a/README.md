Zap Ruder
=========

Zap Ruder is a Zapper test program.

Background
----------
On November 22, 1963, Abraham Zapruder used a Bell & Howell 8mm
camera to shoot a 27-second silent short film of President John F.
Kennedy's limousine and managed to catch his assassination by a
sniper.  The developed film was used in the investigation of this
heinous murder.

The Zapper is a light gun for the NES made by Nintendo, commonly used
for the game *Duck Hunt*.  Just as Zapruder's camera recorded light
from the scene, the Zapper works by measuring light from the TV.
It contains a trigger switch and a photosensor that detects whether
or not the barrel is aimed at a bright area on the screen.

The photosensor is connected to a resonator that distinguishes light
from a CRT SDTV, which flickers at the 15.7 kHz horizontal scan rate,
from any other light source.  As soon as it detects the appropriate
rate, it activates a signal on the controller port.  Because this
time is proportional to how far down the barrel is pointed, one can
think of this signal as using [pulse-position modulation].  By using
timed code, the NES CPU can count how long it takes between the start
of a frame and when the photosensor begins to receive light and
estimate the barrel's position that way.

But very few NES homebrew games use the Zapper.  I suspect that this
might have three causes:

 1. Proliferation of LCD HDTVs, as the Zapper's photosensor works
    only with the CRT SDTVs popular during the NES's commercial era.
 2. Little published source code for how to make the most of
    the Zapper.
 3. Inaccurate emulation of the Zapper in popular emulators.

Cause 1 will only get worse, but I released Russian Roulette
as a tech demo of reading the trigger only.  Zap Ruder aims
to solve cause 2 and provide test cases for solving cause 3.

This demo is for NTSC NES and NTSC TV.  It should also run on RGB
modded NES.  It might even work on PAL famiclones such as Dendy for
what it's worth.  But expect noticeable mistracking on a PAL NES due
to a different CPU clock ratio.  The serial gun of *Vs. Duck Hunt*
is not yet supported.

[pulse-position modulation]: https://en.wikipedia.org/wiki/Pulse-position_modulation

The menu
--------
The menu demonstrates the common technique of flickering the targets
to determine which target the player is pointing at.  When the
photosensor is moved from a dark area to a light area, the menu
darkens the left column and the right column in sequence.  Then it
measures how far down the photosensor is using the "yonoff" kernel
(see below).

If this menu doesn't track well on your TV or your emulator, you
can use the Control Pad of the controller in port 1 instead.

Technical tests
---------------
All tests use a gun in port 2 and a standard controller in port 1
unless otherwise specified.  All tracking tests use the "yonoff"
kernel, which indicates how far down the screen the brightness
started and stopped being detected for the gun in port 2, unless
otherwise specified.

Basic tracking tests use a dithered background of two colors.
Hue is 0 (gray) or 1-12 (color), and brightness is 0 to 8.
Pattern tests use solid-colored objects; brightness can be set only
to odd values.  The colors can also be used to calibrate an NTSC TV's
tint knob: 4 should be magenta (R=B, minimal G), and 10 should be
green (G, minimal R=B).  Pulling the trigger twice rapidly will close
each tracking test.

### Y tracking

Most of the screen is filled with flat dither whose brightness and
hue can be adjusted.

### Two-gun Y tracking

Kernel: yon2p (dual gun)

Most of the screen is filled with flat dither whose brightness and
hue can be adjusted.  This test indicates the start of brightness for
guns in both controller ports; the end of brightness is not measured.

### X tracking

Kernel: xyon

Most of the screen is filled with flat dither whose brightness and
hue can be adjusted.  This test indicates both the horizontal and
vertical position of the start of brightness, though the horizontal
position is very noisy.

The intent of this experimental kernel was to estimate the horizontal
position in 18-pixel units, but noise caused the actual detection
onset to vary by six units.  Thus this kernel isn't very useful.

### Pattern tests

Most of the screen is filled with vertical or horizontal line
patterns whose brightness and hue can be adjusted, but brightness
can be set only to odd values (which correspond to flat colors).

1 of 8, 2 of 8, 3 of 8, 4 of 8
5 of 8, 6 of 8, 7 of 8, 8 of 8
1 of 4, 2 of 4, 3 of 4, 1 of 2

Color can be set to any hue and odd brightness; the rest is black.

### Ball tests

In the center of the screen is a circle whose size, brightness, and
hue can be adjusted, to show how small of a target the photosensor
can reliably detect.  As with the vertical and horizontal line
pattern tests, brightness can be set only to odd values.

### Trigger test

The Zapper contains a mechanism to release the trigger switch once
the trigger is pulled all the way, but one can keep the switch on by
holding the trigger halfway.  This is a simple test that counts
how long the trigger switch on the gun in port 2 is held before it
is released.  This is the only test that works on an HDTV.

Because the Zapper pulls the trigger down harder than the Control
Deck can pull it back up, the switch will usually appear active for
at least 5 frames.

Toys
----

### Axe

Point the gun at the screen to play notes.  Pitch is proportional
to height on a pentatonic scale.  Create rhythm by covering and
uncovering the barrel.  Axe will store your performance and echo
it back to you eight measures later.  Hold the trigger halfway to
change the timbre; press it quickly to accent a note.

To exit, shoot offscreen twice.

Games
-----

### ZapPing

Kernel: yon2p (dual gun)

You've played Pong, the primitive air hockey simulator that was the
first popular video game.  You may have played Odyssey, which was
Pong before Pong was cool.  You may have even played FlapPing for
Atari 2600.  But it is the 2010s, and there is time for ZapPing.
It manages to coax a control feel nearly as smooth as a Wii Remote
from 1985 technology.  But the basic rule of air hockey remains:
avoid missing ball for high score.

At the title screen, press A or pull the trigger to join.  Press the
button again to play against the NES, or press the button on the
other controller to play with two players.

The two player characters are named Podge, in gray, and Daffle, in
red.  These correspond to the two colors of Zapper that Nintendo
has sold, both before and after a change to United States toy safety
regulations.

Move your paddle up and down with the Zapper or by pressing Up or
Down on the Control Pad.  A players with a Zapper will have an unfair
advantage over a player with a controller, just as first-person
shooter players with a mouse have an advantage over players with an
Xbox controller.  Just remember not to point your Zapper off the
big green table because it will stop tracking until you point it at
the table again.  (The Arkanoid controller and Super NES Mouse are
not yet supported due to lack of time, and Power Glove controllers
are not supported yet due to lack of hardware with which to test.
Donations are welcome.)

Each player serves the ball with the A button or the trigger for
two balls before passing.  The game speeds up gradually.  When you
miss the ball, the other player gets a point, and the next serve will
lose some speed but quickly catch up.  The winner is the player with
at least 11 points and ahead by two, except at 20-20, 21 always wins.

To exit, press B on controller 1 at the title screen, or press Reset
if two guns are connected.

Contact
-------
Apart from filing an issue, the most reliable ways to contact the
maintainer about Zap Ruder are by replying to a [topic on NESdev BBS]
or by leaving comments on the project's [talk page].

[topic on NESdev BBS]: https://forums.nesdev.com/viewtopic.php?t=8108
[talk page]: https://pineight.com/mw/?title=Talk:Zap_Ruder

Legal
-----
Copyright 2012, 2019 Damian Yerrick

Copying and distribution of this file, with or without
modification, are permitted in any medium without royalty provided
the copyright notice and this notice are preserved in all source
code copies.  This file is offered as-is, without any warranty.

