This program tests up to two Super NES controllers connected to an
FC Twin console (compatible with NES and Super NES games) or to an
NES Control Deck through a Super NES controller to NES adapter.
I've successfully used my NES with a Nintendo Super NES Controller
(model SNS-005), an asciiPad (model 4900), and a Capcom Fighter
Power Stick.

Instructions
------------

It's already configured for you. Install Python 3, Pillow, GNU Make,
and cc65, then type

    make

Cartridge: NROM-128 with CHR RAM.  Can also run on CHR ROM carts
with the first 4K filled with the contents of `obj/nes/padgfx.chr`.

Once it's running, two Super NES Controllers will appear on screen.
Press buttons on your controllers to make the corresponding buttons
and Control Pad directions of the on-screen controllers light up.

If A, X, L, R, and four buttons are lit on both sides:  
You are using an NES controller.

Legal
-----

Copyright 2016 Damian Yerrick

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE. 