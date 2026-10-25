Test your VRC6!
Choose either vrc6test24 (Dracula) or vrc6test26 (Esper, Madara).

Tests take about 5 seconds to run before any output is shown.

If tests succeeded, this will be shown:

"All Tests Passed!"

If any test failed, this will be shown:

"Test Failed:"
"nn nn nn nn"

The four numbers are both hexidecimal.
The first number indicates what the value of $b003 was when the test failed.
The second number indicates what PPU 1K address increment was being scanned
when the test failed.  For instance, 05 corresponds to PPU $1400:$17ff.
The third number indicates the bank that was expected to be seen at that address.
Chr ROM 1K banks are labeled 00:0f; NTA is labelled 80, and NTB is labelled 81.
The fourth number is the bank that was actually seen at that address.
