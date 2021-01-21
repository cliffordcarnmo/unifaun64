	!address{
		characterindex = $64
		rowindex = $65
		cursorcolorindex = $66
		delayindex = $67
		randomindex = $68
		colormempointer = $80
		screenmempointer = $82
		textcolorpointer = $84
		textpointer = $86
		screenmem = $0400
		textscreenmem = screenmem + 10 * 40
		line1 = screenmem + 9 * 40
		line2 = screenmem + 24 * 40
		sid = $1000
		sidplay = $1000 + 3
		logobitmap = $2000
		logocolors = $3000
		logoscreen = $33eb
		charset = $3800
		sprites = $3e00
		textcolors = $8000
		linecolors = $8281
		cursorcolors = $82aa
		spritecolors = $82be
		text = $8300
		random = $9000
		colormem = $d800
		line1colormem = colormem + 9 * 40
		line2colormem = colormem + 24 * 40
		textcolormem = $d990
	}

	*= $0801

	jsr $e544

	lda #$00
	sta $d020
	sta $d021

	lda #$7f
	sta delayindex

	jsr initsprites

pointers:
	jsr sid
	jsr setspritepointers
	jsr setpointers

logo:
	ldx #$00
-
	lda logocolors,x
	sta colormem,x

	lda logoscreen,x
	sta screenmem,x

	inx
	cpx #$ff
	bne -

	ldx #$00
-
	lda logocolors + 256,x
	sta colormem + 256,x

	lda logoscreen + 256,x
	sta screenmem + 256,x

	inx
	cpx #$3e
	bne -

horizontallines:
	ldx #$00
-
	lda #$63
	sta line1,x
	sta line2,x

	inx
	cpx #40
	bne -

main:
	sei

	lda #%00110101
	sta $01

	lda #%01111111
	sta $dc0d
	sta $dd0d

	lda #%00000001
	sta $d01a

	lda #$00
	sta $d012

	lda #<gfxirq 
	sta $fffe
	lda #>gfxirq
	sta $ffff

	cli

	jmp *

initsprites:
	lda #%00000001
	sta $d010

	lda	#%11111111
	sta $d015
	sta $d01b
	
	lda #%00000000
	sta $d017
	sta $d01c
	sta $d01d

	lda #$02
	sta $d027
	sta $d028
	sta $d029
	sta $d02a
	sta $d02b
	sta $d02c
	sta $d02d
	sta $d02e

	lda #88
	sta $d000
	ldx randomindex
	lda random,x
	sta $d001
	inc randomindex
	rts

gfxirq:
	asl $d019

	jsr sidplay
	;jsr movesprites

	lda #%00111011
	sta $d011

	lda #%00011000
	sta $d016

	lda #%00011110
	sta $d018

	lda #118
	sta $d012

	lda #<txtirq
	sta $fffe
	lda #>txtirq
	sta $ffff
	rti

txtirq:
	asl $d019

	lda #%11001000
	sta $d016

	lda #%00011011
	sta $d011

	lda #%00011110
	sta $d018

	jsr colorcycle
	jsr textwriter

	lda #$00
	sta $d012

	lda #<gfxirq
	sta $fffe
	lda #>gfxirq
	sta $ffff
	rti

movesprites:
	dec $d000

	lda $d010
	cmp #%00000000
	beq lower
	bcs higher
	rts

lower:
	lda $d000
	cmp #$00
	beq resetspritehigh
	rts

higher:
	lda $d000
	cmp #$00
	beq resetspritelow
	rts

resetspritehigh:
	lda #%00000001
	sta $d010

	lda #88
	sta $d000

	ldx randomindex
	lda random,x
	sta $d001

	inc randomindex
	rts

resetspritelow:
	lda #%00000000
	sta $d010

	lda #$ff
	sta $d000
	rts

colorcycle:
	lda linecolors
	sta linecolors + 40

	ldx #$00
-	lda linecolors + 1,x
	sta linecolors,x
	sta line1colormem,x
	sta line2colormem,x

	inx
	cpx #40
	bne -
	rts

textwriter:
	lda cursorcolorindex
	cmp #19
	beq resetcursorcolor

	inc cursorcolorindex

	lda delayindex
	cmp #$00
	beq dotext
	bcs pause
	rts

pause:
	jsr writecursor

	dec delayindex

	lda delayindex
	cmp #$00
	beq clearscreen
	rts

clearscreen:
	lda #$20
	ldx #$00
-
	sta textscreenmem,x
	sta textscreenmem + 255,x

	inx
	cpx #$ff
	bne -

	ldx #$00
-
	sta textscreenmem + 255 * 2,x

	inx
	cpx #50
	bne -

	jsr clearindices
	jsr setcolorpointer
	jsr settextcolorpointer
	jsr setscreenpointer
	rts

dotext:
	jsr writecharacter
	jsr writecursor
	rts

resetcursorcolor:
	lda #$00
	sta cursorcolorindex
	rts

writecursor:
	ldx cursorcolorindex
	ldy characterindex
	lda cursorcolors,x
	sta (colormempointer),y

	lda #$7f
	sta (screenmempointer),y
	rts

writecharacter:
	ldy characterindex
	lda (textcolorpointer),y
	sta (colormempointer),y

	lda (textpointer),y
	cmp #$ff
	beq setpointers

	sta (screenmempointer),y

	cpy #39
	beq nextrow

	inc characterindex
	rts

pushoffsets:
	clc

	lda colormempointer
	adc #40
	sta colormempointer
	lda colormempointer + 1
	adc #$00
	sta colormempointer + 1

	lda textcolorpointer
	adc #40
	sta textcolorpointer
	lda textcolorpointer + 1
	adc #$00
	sta textcolorpointer + 1

	lda screenmempointer
	adc #40
	sta screenmempointer
	lda screenmempointer + 1
	adc #$00
	sta screenmempointer + 1
	rts

pushoffsets2:
	clc

	lda textpointer
	adc #40
	sta textpointer
	lda textpointer + 1
	adc #$00
	sta textpointer + 1
	rts

nextrow:
	jsr pushoffsets2

	lda rowindex
	cmp #12
	beq nextscreen

	jsr pushoffsets

	lda #$00
	sta characterindex

	inc rowindex
	rts

nextscreen:
	lda #$ff
	sta delayindex
	rts

;reset pointers and indices
setpointers:
	jsr clearindices
	jsr setcolorpointer
	jsr setscreenpointer
	jsr settextcolorpointer
	jsr settextpointer
	rts

clearindices:
	lda #$00
	sta characterindex
	sta rowindex
	sta cursorcolorindex
	rts

setcolorpointer:
	lda #<textcolormem
	sta colormempointer
	lda #>textcolormem
	sta colormempointer + 1
	rts

setscreenpointer:
	lda #<textscreenmem
	sta screenmempointer
	lda #>textscreenmem
	sta screenmempointer + 1
	rts

settextcolorpointer:
	lda #<textcolors
	sta textcolorpointer
	lda #>textcolors
	sta textcolorpointer + 1
	rts

settextpointer:
	lda #<text
	sta textpointer
	lda #>text
	sta textpointer + 1
	rts

setspritepointers:
	lda #$f8
	sta $07f8
	lda #$f9
	sta $07f9
	lda #$fa
	sta $07fa
	lda #$fb
	sta $07fb
	lda #$fc
	sta $07fc
	lda #$fd
	sta $07fd
	lda #$fe
	sta $07fe
	lda #$ff
	sta $07ff
	rts

	*= sid
	!binary "data/goldie.sid",,$7e

	*= logobitmap
	!binary "data/unifaun.kla",2540,2

	*= logoscreen
	!binary "data/unifaun.kla",318,8002

	*= logocolors
	!binary "data/unifaun.kla",297,9002

	*= charset
	!byte $3c, $66, $6e, $6e, $60, $62, $3c, $00
	!byte $3c, $66, $66, $66, $7e, $66, $66, $00
	!byte $7c, $66, $66, $7c, $66, $66, $7c, $00
	!byte $3e, $60, $60, $60, $60, $60, $3e, $00
	!byte $7c, $66, $66, $66, $66, $66, $7c, $00
	!byte $3e, $60, $60, $78, $60, $60, $3e, $00
	!byte $3e, $60, $60, $78, $60, $60, $60, $00
	!byte $3c, $66, $60, $6e, $66, $66, $3c, $00
	!byte $66, $66, $66, $7e, $66, $66, $66, $00
	!byte $18, $18, $18, $18, $18, $18, $18, $00
	!byte $0c, $0c, $0c, $0c, $0c, $0c, $78, $00
	!byte $66, $6c, $78, $70, $78, $6c, $66, $00
	!byte $60, $60, $60, $60, $60, $60, $3e, $00
	!byte $63, $77, $7f, $6b, $63, $63, $63, $00
	!byte $66, $76, $7e, $7e, $6e, $66, $66, $00
	!byte $3c, $66, $66, $66, $66, $66, $3c, $00
	!byte $7c, $66, $66, $66, $66, $7c, $60, $00
	!byte $3c, $66, $66, $66, $66, $3c, $0e, $00
	!byte $7c, $66, $66, $7c, $78, $6c, $66, $00
	!byte $3c, $66, $60, $3c, $06, $66, $3c, $00
	!byte $7e, $18, $18, $18, $18, $18, $18, $00
	!byte $66, $66, $66, $66, $66, $66, $3c, $00
	!byte $66, $66, $66, $66, $66, $3c, $18, $00
	!byte $63, $63, $63, $6b, $7f, $77, $63, $00
	!byte $66, $66, $3c, $18, $3c, $66, $66, $00
	!byte $66, $66, $66, $3c, $18, $18, $18, $00
	!byte $7e, $06, $0c, $18, $30, $60, $7e, $00
	!byte $3c, $30, $30, $30, $30, $30, $3c, $00
	!byte $0c, $12, $30, $7c, $30, $62, $fc, $00
	!byte $3c, $0c, $0c, $0c, $0c, $0c, $3c, $00
	!byte $00, $18, $3c, $7e, $18, $18, $18, $18
	!byte $00, $10, $30, $7f, $7f, $30, $10, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $18, $18, $18, $18, $00, $00, $18, $00
	!byte $66, $66, $66, $00, $00, $00, $00, $00
	!byte $66, $66, $ff, $66, $ff, $66, $66, $00
	!byte $18, $3e, $60, $3c, $06, $7c, $18, $00
	!byte $62, $66, $0c, $18, $30, $66, $46, $00
	!byte $3c, $66, $3c, $38, $67, $66, $3f, $00
	!byte $06, $0c, $18, $00, $00, $00, $00, $00
	!byte $0c, $18, $30, $30, $30, $18, $0c, $00
	!byte $30, $18, $0c, $0c, $0c, $18, $30, $00
	!byte $00, $66, $3c, $ff, $3c, $66, $00, $00
	!byte $00, $18, $18, $7e, $18, $18, $00, $00
	!byte $00, $00, $00, $00, $00, $18, $18, $30
	!byte $00, $00, $00, $7e, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $18, $18, $00
	!byte $00, $03, $06, $0c, $18, $30, $60, $00
	!byte $3c, $66, $6e, $76, $66, $66, $3c, $00
	!byte $18, $18, $38, $18, $18, $18, $7e, $00
	!byte $3c, $66, $06, $0c, $30, $60, $7e, $00
	!byte $3c, $66, $06, $1c, $06, $66, $3c, $00
	!byte $06, $0e, $1e, $66, $7f, $06, $06, $00
	!byte $7e, $60, $7c, $06, $06, $66, $3c, $00
	!byte $3c, $66, $60, $7c, $66, $66, $3c, $00
	!byte $7e, $66, $0c, $18, $18, $18, $18, $00
	!byte $3c, $66, $66, $3c, $66, $66, $3c, $00
	!byte $3c, $66, $66, $3e, $06, $66, $3c, $00
	!byte $00, $00, $18, $00, $00, $18, $00, $00
	!byte $00, $00, $18, $00, $00, $18, $18, $30
	!byte $0e, $18, $30, $60, $30, $18, $0e, $00
	!byte $00, $00, $7e, $00, $7e, $00, $00, $00
	!byte $70, $18, $0c, $06, $0c, $18, $70, $00
	!byte $3c, $66, $06, $0c, $18, $00, $18, $00
	!byte $00, $00, $00, $ff, $ff, $00, $00, $00
	!byte $08, $1c, $3e, $7f, $7f, $1c, $3e, $00
	!byte $18, $18, $18, $18, $18, $18, $18, $18
	!byte $00, $00, $00, $ff, $ff, $00, $00, $00
	!byte $00, $00, $ff, $ff, $00, $00, $00, $00
	!byte $00, $ff, $ff, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $ff, $ff, $00, $00
	!byte $30, $30, $30, $30, $30, $30, $30, $30
	!byte $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c
	!byte $00, $00, $00, $e0, $f0, $38, $18, $18
	!byte $18, $18, $1c, $0f, $07, $00, $00, $00
	!byte $18, $18, $38, $f0, $e0, $00, $00, $00
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $ff, $ff
	!byte $c0, $e0, $70, $38, $1c, $0e, $07, $03
	!byte $03, $07, $0e, $1c, $38, $70, $e0, $c0
	!byte $ff, $ff, $c0, $c0, $c0, $c0, $c0, $c0
	!byte $ff, $ff, $03, $03, $03, $03, $03, $03
	!byte $00, $3c, $7e, $7e, $7e, $7e, $3c, $00
	!byte $00, $00, $00, $00, $00, $ff, $ff, $00
	!byte $36, $7f, $7f, $7f, $3e, $1c, $08, $00
	!byte $60, $60, $60, $60, $60, $60, $60, $60
	!byte $00, $00, $00, $07, $0f, $1c, $18, $18
	!byte $c3, $e7, $7e, $3c, $3c, $7e, $e7, $c3
	!byte $00, $3c, $7e, $66, $66, $7e, $3c, $00
	!byte $18, $18, $66, $66, $18, $18, $3c, $00
	!byte $06, $06, $06, $06, $06, $06, $06, $06
	!byte $08, $1c, $3e, $7f, $3e, $1c, $08, $00
	!byte $18, $18, $18, $ff, $ff, $18, $18, $18
	!byte $c0, $c0, $30, $30, $c0, $c0, $30, $30
	!byte $18, $18, $18, $18, $18, $18, $18, $18
	!byte $00, $00, $03, $3e, $76, $36, $36, $00
	!byte $ff, $7f, $3f, $1f, $0f, $07, $03, $01
	!byte $00, $00, $00, $00, $00, $00, $00, $00
	!byte $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0
	!byte $00, $00, $00, $00, $ff, $ff, $ff, $ff
	!byte $ff, $00, $00, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $00, $00, $ff
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0
	!byte $cc, $cc, $33, $33, $cc, $cc, $33, $33
	!byte $03, $03, $03, $03, $03, $03, $03, $03
	!byte $00, $00, $00, $00, $cc, $cc, $33, $33
	!byte $ff, $fe, $fc, $f8, $f0, $e0, $c0, $80
	!byte $03, $03, $03, $03, $03, $03, $03, $03
	!byte $18, $18, $18, $1f, $1f, $18, $18, $18
	!byte $00, $00, $00, $00, $0f, $0f, $0f, $0f
	!byte $18, $18, $18, $1f, $1f, $00, $00, $00
	!byte $00, $00, $00, $f8, $f8, $18, $18, $18
	!byte $00, $00, $00, $00, $00, $00, $ff, $ff
	!byte $00, $00, $00, $1f, $1f, $18, $18, $18
	!byte $18, $18, $18, $ff, $ff, $00, $00, $00
	!byte $00, $00, $00, $ff, $ff, $18, $18, $18
	!byte $18, $18, $18, $f8, $f8, $18, $18, $18
	!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0
	!byte $e0, $e0, $e0, $e0, $e0, $e0, $e0, $e0
	!byte $07, $07, $07, $07, $07, $07, $07, $07
	!byte $ff, $ff, $00, $00, $00, $00, $00, $00
	!byte $ff, $ff, $ff, $00, $00, $00, $00, $00
	!byte $00, $00, $00, $00, $00, $ff, $ff, $ff
	!byte $03, $03, $03, $03, $03, $03, $ff, $ff
	!byte $00, $00, $00, $00, $f0, $f0, $f0, $f0
	!byte $0f, $0f, $0f, $0f, $00, $00, $00, $00
	!byte $0f, $0f, $0f, $0f, $0f, $0f, $0f, $0f
	!byte $3f, $3f, $3f, $3f, $3f, $3f, $3f, $3f
	!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

	*= sprites
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111
	!byte %11111111,%11111111,%11111111

	*= textcolors
	!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
	!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
	!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b

	*= linecolors
	!byte $0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0f,$0f,$05,$05,$07,$07,$03,$03,$01,$01,$01,$01,$01,$01,$01,$01,$03,$03,$07,$07,$05,$05,$0f,$0f,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0b

	*= cursorcolors
	!byte $00,$00,$0b,$0b,$0c,$0c,$0f,$0f,$03,$01,$01,$03,$0f,$0f,$0c,$0c,$0b,$0b,$00,$00

	*= spritecolors
	!byte $0b,$0c,$0f,$07,$01

	*= text
	!scr "  SunifaunS is a market leader within   "
	!scr " transport management(tm) on the nordic "
	!scr "   market. with more than 20 years of   "
	!scr "  experience we provide innovative tm   "
	!scr "  systems of high quality that simplify "
	!scr " and improve processes for the transport"
	!scr "    buyer as well as for the carrier.   "
	!scr "      more than 800 000 shipments       "
	!scr "           registered by over           "
	!scr "     100 000 companies flow through     "
	!scr "    our tm systems on a daily basis     "
	!scr "   which makes us a central part of     "
	!scr "         the nordic logistics.          "

	!scr "  we have built the swedish tm-market   "
	!scr "   and has actively driven it to the    "
	!scr " world-leading level of edi the country "
	!scr " has today. we are now going through    "
	!scr " the same journey in finland, norway,   "
	!scr "         poland and denmark.            "
	!scr "   unifaun has an outspoken plan for    "
	!scr "  international expansion and our goal  "
	!scr "   is to be market leading in europe.   "
	!scr "         by being a leading             "
	!scr "   technology- and knowledge partner    "
	!scr " unifaun wants to ensure that logistics "
	!scr "     is an enabler for business.        "

	!scr "   we are always looking for talented   "
	!scr "    software engineers, architects,     "
	!scr "       integration specialists,         "
	!scr "       operations engineers and         "
	!scr "           data specialists.            "
	!scr "         we enjoy working with          "
	!scr "          open source software          "
	!scr "    and embrace engineering culture.    "
	!scr "       we are located in both           "
	!scr "      gothenburg and stockholm.         "
	!scr "   come visit us for a cup of coffee    "
	!scr "           or contact us at             "
	!scr "            hr@unifaun.com              "

	!scr "                                        "
	!scr "              unifaun64                 "
	!scr "                                        "
	!scr "         code and graphics by           "
	!scr "        clifford 'randy' carnmo         "
	!scr "                                        "
	!scr "               music by                 "
	!scr "       stellan 'dane' andersson         "
	!scr "                                        "
	!scr "       greetings to the unifaun         "
	!scr "         S engineering team S           "
	!scr "                                        "
	!scr "                                        "

	!byte $ff

	*= random
	!source "data/rnd.asm"