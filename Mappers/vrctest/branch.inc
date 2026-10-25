.macro tbpl arg
bpl arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbmi arg
bmi arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbvc arg
bvc arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbvs arg
bvs arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbcc arg
bcc arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbcs arg
bcs arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbne arg
bne arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro tbeq arg
beq arg
.assert >* = >arg, error, "Timing-critical branch crosses page boundary"
.endmacro

.macro pbpl arg
bpl arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbmi arg
bmi arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbvc arg
bvc arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbvs arg
bvs arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbcc arg
bcc arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbcs arg
bcs arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbne arg
bne arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro

.macro pbeq arg
beq arg
.assert >* = >arg, warning, "Performance-sensitive branch crosses page boundary"
.endmacro
