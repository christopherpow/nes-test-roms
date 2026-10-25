supercat's collision idiom
==========================

A [post by supercat] on AtariAge forum in 2006 gives an efficient
idiom on the 6502 for comparing two fixed-size intervals for
overlap given the intervals' left sizes.  This can prove useful
for bounding box collision detection in a game.

    clc
    lda left1
    sbc left2  ; A = left1 - left2 - 1; CF set if left1 to right
    sbc #WIDTH2 - 1
    adc #WIDTH1 + WIDTH2 - 1
    bcs is_overlap

[post by supercat]: https://forums.atariage.com/topic/71120-6502-killer-hacks/?do=findComment&comment=1054049
