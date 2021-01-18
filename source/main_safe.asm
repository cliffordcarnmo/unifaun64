	;Unifaun64
	;Code and graphics by Clifford "Randy" Carnmo
	;Music by Stellan "Dane" Andersson

	;$f4 = Cursor color counter
	;$f5 = Color memory pointer low byte 
	;$f6 = Color memory pointer high byte 
	;$f7 = Text color memory pointer
	;$f8 = Text color memory pointer
	;$f9 = Character counter
	;$fa = Row counter
	;$fb = Screen pause counter
	;$fc = Screen memory pointer low byte
	;$fd = Screen memory pointer high byte
	;$fe = Text data memory pointer low byte
	;$ff = Text data memory pointer high byte

	;Code entry point
	*= $0801

	;Lazy kernal function for initial screen clear (https://sta.c64.org/cbm64krnfunc.html)
	jsr $e544

	;Initialize working bytes
	lda #$00
	sta $f4
	sta $f5
	sta $f6
	sta $f7
	sta $f8
	sta $f9
	sta $fa
	sta $fb
	sta $fc
	sta $fd
	sta $fe
	sta $ff

	;Black background color
	sta $d020
	sta $d021

	;Initialize sid
	jsr $1000

	;Initialize pointers
	jsr setpointers

;Initialize logo
	ldx #$00
-
	;Get logo color data + offset
	lda $6000,x

	;Put it in color memory + offset
	sta $d800,x

	;Get logo character data + offset
	lda $63eb,x

	;Put it in screen memory + offset
	sta $0400,x

	inx
	cpx #$ff
	bne -

	;Continue copying logo data
	ldx #$00
-
	lda $6100,x
	sta $d900,x

	lda $64eb,x
	sta $0500,x

	inx
	cpx #$3e
	bne -

;Initialize horizontal lines
	ldx #$00
-
	lda #$63
	sta $0568,x
	sta $0568+40*15,x

	inx
	cpx #40
	bne -

	;Disable interrupts
	sei

	;Set cpu memory configuration (http://sta.c64.org/cbm64mem.html)
	lda #$35
	sta $01

	;Configure interrupt control registers
	lda #$7f
	sta $dc0d
	sta $dd0d

	lda #$01
	sta $d01a

	;Horizontal raster line to trigger interrupt
	lda #$00
	sta $d012

	;Put address of interrupt in jump vector
	lda #<gfxirq 
	sta $fffe
	lda #>gfxirq
	sta $ffff

	;Enable interrupts
	cli

	;Dummy infinite loop
	jmp *

;Irq for the top graphics part of the screen
gfxirq:
	;Acknowledge interrupt
	asl $d019

	;Jump to sid play routine
	jsr $1003

	;Configure registers for multicolor bitmap mode
	lda #$3b
	sta $d011

	lda #$18
	sta $d016

	lda #%00011110
	sta $d018

	;Horizontal raster line to trigger next interrupt
	lda #118
	sta $d012

	;Put address of interrupt in jump vector
	lda #<txtirq
	sta $fffe
	lda #>txtirq
	sta $ffff
	rti

;Irq for the lower text part of the screen
txtirq:
	;Acknowledge interrupt
	asl $d019

	;Configure registers for standard text mode
	lda #%11001000
	sta $d016

	lda #%00011011
	sta $d011

	lda #%00011110
	sta $d018

	;Call the colorcycler and textwriter every frame in this interrupt
	jsr colorcycle
	jsr textwriter

	;Horizontal raster line to trigger next interrupt
	lda #$00
	sta $d012

	;Put address of interrupt in jump vector
	lda #<gfxirq
	sta $fffe
	lda #>gfxirq
	sta $ffff
	rti

;Get color data and cycle the list
colorcycle:
	lda $8281
	sta $8281+40

	ldx #$00
-
	lda $8281+1,x
	sta $8281,x
	sta $d968,x
	sta $d968+40*15,x

	;Repeat for every character in the row
	inx
	cpx #40
	bne -
	rts

;Clear the text area by overwriting it with blank characters
cleartextarea:
	lda #$20
	ldx #$00
-
	sta $0590,x
	sta $0590+255,x

	inx
	cpx #$ff
	bne -

	ldx #$00
-
	sta $0590+255*2,x

	inx
	cpx #130-40*2
	bne -
	rts

textwriter:
	lda $f4
	cmp #20
	beq resetcursorcolor

	;Increase the cursor color counter
	inc $f4

	;Check for screen pause
	lda $fb
	cmp #$00
	beq dotext

	lda $fb
	cmp #$00
	bcs pause
	rts

pause:
	;Write a cursor
	jsr writecursor

	;Decrease screen pause counter
	dec $fb
	
	;If the pause is over, clear the screen
	lda $fb
	cmp #$00
	beq cleartextarea
	rts

dotext:
	;Write a character 
	jsr writecharacter

	;Write a cursor
	jsr writecursor
	rts

resetcursorcolor:
	lda #$00
	sta $f4
	rts

;Write a cursor
writecursor:
	;Get cursor color index
	ldx $f4

	;Get current character index
	ldy $f9

	;Get color from cursor color memory + offset
	lda $82aa,x
	
	;Write it in the color memory pointer + offset
	sta ($f5),y

	;Pick a character that looks like a cursor (http://sta.c64.org/cbm64pet.html)
	lda #$e0

	;Write the character to screen memory pointer + offset + 1 (since we already increased the character index)
	sta ($fc),y
	rts

;Write characters on the screen
writecharacter:
	;Get current character index
	ldy $f9

	;Get color from text color memory pointer + offset
	lda ($f7),y

	;Put it in color memory pointer + offset
	sta ($f5),y

	;Get corresponding character from text data memory pointer + offset
	lda ($fe),y

	;Check if the byte is $ff
	cmp #$ff

	;If it is, we have wrote all screens, jump to restart
	beq restart

	;Write the character to screen memory pointer + offset
	sta ($fc),y

	;Check if it is 40 columns (we have reached the end of a row)
	cpy #39

	;If it is, advance a row and clear the character index
	beq nextrow

	;Increase character index
	inc $f9
	rts

;Advance rows
nextrow:
	;Clear carry
	clc

	;Add #40 (a row of bytes) to the color memory pointer ($f5 & $f6) as offset
	lda $f5
	adc #40
	sta $f5

	lda $f6
	adc #$00
	sta $f6

	;Add #40 (a row of bytes) to the text color data pointer ($f7 & $f8) as offset
	lda $f7
	adc #40
	sta $f7

	lda $f8
	adc #$00
	sta $f8

	;Add #40 (a row of bytes) to the screen memory pointer ($fc & $fd) as offset
	lda $fc
	adc #40
	sta $fc

	lda $fd
	adc #$00
	sta $fd

	;Add #40 (a row of bytes) to the text data pointer ($fe & $ff) as offset
	lda $fe
	adc #40
	sta $fe

	lda $ff
	adc #$00
	sta $ff
 
	;Increase row index
	inc $fa

	;Advance to the next screen when we reach 13 rows
	lda $fa
	cmp #13
	beq nextscreen

	;Clear character index
	lda #$00
	sta $f9
	rts

;Add timer value to screen pause counter, reset indices, pointers and clear screen
nextscreen:
	lda #$ff
	sta $fb

	jsr resetcurrentscreen
	rts

;Reset indices, pointers and clear screen
resetcurrentscreen:
	jsr clearindices
	jsr setcolorpointer
	jsr settextcolorpointer
	jsr setscreenpointer
	rts

;Reset everything and start over
restart:
	jsr setpointers
	rts

;Reset pointers to initial values
setpointers:
	jsr clearindices
	jsr setcolorpointer
	jsr settextcolorpointer
	jsr setscreenpointer
	jsr settextpointer
	rts

;Initializa text data memory pointer
settextpointer:
	lda #$00
	sta $fe

	lda #$90
	sta $ff
	rts

;Initialize screen memory pointer
setscreenpointer:
	lda #$90
	sta $fc

	lda #$05
	sta $fd
	rts

;Initialize color memory pointer
setcolorpointer:
	lda #$90
	sta $f5

	lda #$d9
	sta $f6
	rts

;Initialize text color memory pointer
settextcolorpointer:
	lda #$00
	sta $f7

	lda #$80
	sta $f8
	rts

clearindices:
	lda #$00
	sta $f9
	sta $fa
	rts

	;Sid data (3854 bytes), https://csdb.dk/sid/?id=49449
	*= $1000
	!binary "data/Goldie.sid",,$7e

	;Logo bitmap data (we only need the first 2540 bytes)
	*= $2000
	!binary "data/unifaun.map",2540

	;Character set data
	*= $3800
	!byte $3c,$66,$6e,$6e,$60,$62,$3c,$00
	!byte $3c,$66,$66,$66,$66,$7e,$66,$00
	!byte $7c,$66,$66,$7c,$66,$66,$7c,$00
	!byte $3e,$60,$60,$60,$60,$60,$3e,$00
	!byte $7c,$66,$66,$66,$66,$66,$7c,$00
	!byte $3e,$60,$60,$78,$60,$60,$3e,$00
	!byte $3e,$60,$60,$78,$60,$60,$60,$00
	!byte $3c,$66,$60,$6e,$66,$66,$3c,$00
	!byte $66,$66,$66,$7e,$66,$66,$66,$00
	!byte $18,$18,$18,$18,$18,$18,$18,$00
	!byte $0c,$0c,$0c,$0c,$0c,$0c,$78,$00
	!byte $66,$6c,$78,$70,$78,$6c,$66,$00
	!byte $60,$60,$60,$60,$60,$60,$7e,$00
	!byte $63,$77,$7f,$6b,$63,$63,$63,$00
	!byte $66,$76,$7e,$7e,$6e,$66,$66,$00
	!byte $3c,$66,$66,$66,$66,$66,$3c,$00
	!byte $7c,$66,$66,$66,$66,$7c,$60,$00
	!byte $3c,$66,$66,$66,$66,$3c,$0e,$00
	!byte $7c,$66,$66,$7c,$78,$6c,$66,$00
	!byte $3c,$66,$60,$3c,$06,$66,$3c,$00
	!byte $7e,$18,$18,$18,$18,$18,$18,$00
	!byte $66,$66,$66,$66,$66,$66,$3c,$00
	!byte $66,$66,$66,$66,$66,$3c,$18,$00
	!byte $63,$63,$63,$6b,$7f,$77,$63,$00
	!byte $66,$66,$3c,$18,$3c,$66,$66,$00
	!byte $66,$66,$66,$3c,$18,$18,$18,$00
	!byte $7e,$06,$0c,$18,$30,$60,$7e,$00
	!byte $3c,$30,$30,$30,$30,$30,$3c,$00
	!byte $0c,$12,$30,$7c,$30,$62,$fc,$00
	!byte $3c,$0c,$0c,$0c,$0c,$0c,$3c,$00
	!byte $00,$18,$3c,$7e,$18,$18,$18,$18
	!byte $00,$10,$30,$7f,$7f,$30,$10,$00
	!byte $00,$00,$00,$00,$00,$00,$00,$00
	!byte $18,$18,$18,$18,$00,$00,$18,$00
	!byte $66,$66,$66,$00,$00,$00,$00,$00
	!byte $66,$66,$ff,$66,$ff,$66,$66,$00
	!byte $18,$3e,$60,$3c,$06,$7c,$18,$00
	!byte $62,$66,$0c,$18,$30,$66,$46,$00
	!byte $3c,$66,$3c,$38,$67,$66,$3f,$00
	!byte $06,$0c,$18,$00,$00,$00,$00,$00
	!byte $0c,$18,$30,$30,$30,$18,$0c,$00
	!byte $30,$18,$0c,$0c,$0c,$18,$30,$00
	!byte $00,$66,$3c,$ff,$3c,$66,$00,$00
	!byte $00,$18,$18,$7e,$18,$18,$00,$00
	!byte $00,$00,$00,$00,$00,$18,$18,$30
	!byte $00,$00,$00,$7e,$00,$00,$00,$00
	!byte $00,$00,$00,$00,$00,$18,$18,$00
	!byte $00,$03,$06,$0c,$18,$30,$60,$00
	!byte $3c,$66,$6e,$76,$66,$66,$3c,$00
	!byte $18,$18,$38,$18,$18,$18,$7e,$00
	!byte $3c,$66,$06,$0c,$30,$60,$7e,$00
	!byte $3c,$66,$06,$1c,$06,$66,$3c,$00
	!byte $06,$0e,$1e,$66,$7f,$06,$06,$00
	!byte $7e,$60,$7c,$06,$06,$66,$3c,$00
	!byte $3c,$66,$60,$7c,$66,$66,$3c,$00
	!byte $7e,$66,$0c,$18,$18,$18,$18,$00
	!byte $3c,$66,$66,$3c,$66,$66,$3c,$00
	!byte $3c,$66,$66,$3e,$06,$66,$3c,$00
	!byte $00,$00,$18,$00,$00,$18,$00,$00
	!byte $00,$00,$18,$00,$00,$18,$18,$30
	!byte $0e,$18,$30,$60,$30,$18,$0e,$00
	!byte $00,$00,$7e,$00,$7e,$00,$00,$00
	!byte $70,$18,$0c,$06,$0c,$18,$70,$00
	!byte $3c,$66,$06,$0c,$18,$00,$18,$00
	!byte $00,$00,$00,$ff,$ff,$00,$00,$00
	!byte $08,$1c,$3e,$7f,$7f,$1c,$3e,$00
	!byte $18,$18,$18,$18,$18,$18,$18,$18
	!byte $00,$00,$00,$ff,$ff,$00,$00,$00
	!byte $00,$00,$ff,$ff,$00,$00,$00,$00
	!byte $00,$ff,$ff,$00,$00,$00,$00,$00
	!byte $00,$00,$00,$00,$ff,$ff,$00,$00
	!byte $30,$30,$30,$30,$30,$30,$30,$30
	!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
	!byte $00,$00,$00,$e0,$f0,$38,$18,$18
	!byte $18,$18,$1c,$0f,$07,$00,$00,$00
	!byte $18,$18,$38,$f0,$e0,$00,$00,$00
	!byte $c0,$c0,$c0,$c0,$c0,$c0,$ff,$ff
	!byte $c0,$e0,$70,$38,$1c,$0e,$07,$03
	!byte $03,$07,$0e,$1c,$38,$70,$e0,$c0
	!byte $ff,$ff,$c0,$c0,$c0,$c0,$c0,$c0
	!byte $ff,$ff,$03,$03,$03,$03,$03,$03
	!byte $00,$3c,$7e,$7e,$7e,$7e,$3c,$00
	!byte $00,$00,$00,$00,$00,$ff,$ff,$00
	!byte $36,$7f,$7f,$7f,$3e,$1c,$08,$00
	!byte $60,$60,$60,$60,$60,$60,$60,$60
	!byte $00,$00,$00,$07,$0f,$1c,$18,$18
	!byte $c3,$e7,$7e,$3c,$3c,$7e,$e7,$c3
	!byte $00,$3c,$7e,$66,$66,$7e,$3c,$00
	!byte $18,$18,$66,$66,$18,$18,$3c,$00
	!byte $06,$06,$06,$06,$06,$06,$06,$06
	!byte $08,$1c,$3e,$7f,$3e,$1c,$08,$00
	!byte $18,$18,$18,$ff,$ff,$18,$18,$18
	!byte $c0,$c0,$30,$30,$c0,$c0,$30,$30
	!byte $18,$18,$18,$18,$18,$18,$18,$18
	!byte $00,$00,$03,$3e,$76,$36,$36,$00
	!byte $ff,$7f,$3f,$1f,$0f,$07,$03,$01
	!byte $00,$00,$00,$00,$00,$00,$00,$00
	!byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
	!byte $00,$00,$00,$00,$ff,$ff,$ff,$ff
	!byte $ff,$00,$00,$00,$00,$00,$00,$00
	!byte $00,$00,$00,$00,$00,$00,$00,$ff
	!byte $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
	!byte $cc,$cc,$33,$33,$cc,$cc,$33,$33
	!byte $03,$03,$03,$03,$03,$03,$03,$03
	!byte $00,$00,$00,$00,$cc,$cc,$33,$33
	!byte $ff,$fe,$fc,$f8,$f0,$e0,$c0,$80
	!byte $03,$03,$03,$03,$03,$03,$03,$03
	!byte $18,$18,$18,$1f,$1f,$18,$18,$18
	!byte $00,$00,$00,$00,$0f,$0f,$0f,$0f
	!byte $18,$18,$18,$1f,$1f,$00,$00,$00
	!byte $00,$00,$00,$f8,$f8,$18,$18,$18
	!byte $00,$00,$00,$00,$00,$00,$ff,$ff
	!byte $00,$00,$00,$1f,$1f,$18,$18,$18
	!byte $18,$18,$18,$ff,$ff,$00,$00,$00
	!byte $00,$00,$00,$ff,$ff,$18,$18,$18
	!byte $18,$18,$18,$f8,$f8,$18,$18,$18
	!byte $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
	!byte $e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
	!byte $07,$07,$07,$07,$07,$07,$07,$07
	!byte $ff,$ff,$00,$00,$00,$00,$00,$00
	!byte $ff,$ff,$ff,$00,$00,$00,$00,$00
	!byte $00,$00,$00,$00,$00,$ff,$ff,$ff
	!byte $03,$03,$03,$03,$03,$03,$ff,$ff
	!byte $00,$00,$00,$00,$f0,$f0,$f0,$f0
	!byte $0f,$0f,$0f,$0f,$00,$00,$00,$00
	!byte $18,$18,$18,$f8,$f8,$00,$00,$00
	!byte $f0,$f0,$f0,$f0,$00,$00,$00,$00
	!byte $f0,$f0,$f0,$f0,$0f,$0f,$0f,$0f
	!byte $c3,$99,$91,$91,$9f,$99,$c3,$ff
	!byte $e7,$c3,$99,$81,$99,$99,$99,$ff
	!byte $83,$99,$99,$83,$99,$99,$83,$ff
	!byte $c3,$99,$9f,$9f,$9f,$99,$c3,$ff
	!byte $87,$93,$99,$99,$99,$93,$87,$ff
	!byte $81,$9f,$9f,$87,$9f,$9f,$81,$ff
	!byte $81,$9f,$9f,$87,$9f,$9f,$9f,$ff
	!byte $c3,$99,$9f,$91,$99,$99,$c3,$ff
	!byte $99,$99,$99,$81,$99,$99,$99,$ff
	!byte $c3,$e7,$e7,$e7,$e7,$e7,$c3,$ff
	!byte $e1,$f3,$f3,$f3,$f3,$93,$c7,$ff
	!byte $99,$93,$87,$8f,$87,$93,$99,$ff
	!byte $9f,$9f,$9f,$9f,$9f,$9f,$81,$ff
	!byte $9c,$88,$80,$94,$9c,$9c,$9c,$ff
	!byte $99,$89,$81,$81,$91,$99,$99,$ff
	!byte $c3,$99,$99,$99,$99,$99,$c3,$ff
	!byte $83,$99,$99,$83,$9f,$9f,$9f,$ff
	!byte $c3,$99,$99,$99,$99,$c3,$f1,$ff
	!byte $83,$99,$99,$83,$87,$93,$99,$ff
	!byte $c3,$99,$9f,$c3,$f9,$99,$c3,$ff
	!byte $81,$e7,$e7,$e7,$e7,$e7,$e7,$ff
	!byte $99,$99,$99,$99,$99,$99,$c3,$ff
	!byte $99,$99,$99,$99,$99,$c3,$e7,$ff
	!byte $9c,$9c,$9c,$94,$80,$88,$9c,$ff
	!byte $99,$99,$c3,$e7,$c3,$99,$99,$ff
	!byte $99,$99,$99,$c3,$e7,$e7,$e7,$ff
	!byte $81,$f9,$f3,$e7,$cf,$9f,$81,$ff
	!byte $c3,$cf,$cf,$cf,$cf,$cf,$c3,$ff
	!byte $f3,$ed,$cf,$83,$cf,$9d,$03,$ff
	!byte $c3,$f3,$f3,$f3,$f3,$f3,$c3,$ff
	!byte $ff,$e7,$c3,$81,$e7,$e7,$e7,$e7
	!byte $ff,$ef,$cf,$80,$80,$cf,$ef,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	!byte $e7,$e7,$e7,$e7,$ff,$ff,$e7,$ff
	!byte $99,$99,$99,$ff,$ff,$ff,$ff,$ff
	!byte $99,$99,$00,$99,$00,$99,$99,$ff
	!byte $e7,$c1,$9f,$c3,$f9,$83,$e7,$ff
	!byte $9d,$99,$f3,$e7,$cf,$99,$b9,$ff
	!byte $c3,$99,$c3,$c7,$98,$99,$c0,$ff
	!byte $f9,$f3,$e7,$ff,$ff,$ff,$ff,$ff
	!byte $f3,$e7,$cf,$cf,$cf,$e7,$f3,$ff
	!byte $cf,$e7,$f3,$f3,$f3,$e7,$cf,$ff
	!byte $ff,$99,$c3,$00,$c3,$99,$ff,$ff
	!byte $ff,$e7,$e7,$81,$e7,$e7,$ff,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$e7,$e7,$cf
	!byte $ff,$ff,$ff,$81,$ff,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$e7,$e7,$ff
	!byte $ff,$fc,$f9,$f3,$e7,$cf,$9f,$ff
	!byte $c3,$99,$91,$89,$99,$99,$c3,$ff
	!byte $e7,$e7,$c7,$e7,$e7,$e7,$81,$ff
	!byte $c3,$99,$f9,$f3,$cf,$9f,$81,$ff
	!byte $c3,$99,$f9,$e3,$f9,$99,$c3,$ff
	!byte $f9,$f1,$e1,$99,$80,$f9,$f9,$ff
	!byte $81,$9f,$83,$f9,$f9,$99,$c3,$ff
	!byte $c3,$99,$9f,$83,$99,$99,$c3,$ff
	!byte $81,$99,$f3,$e7,$e7,$e7,$e7,$ff
	!byte $c3,$99,$99,$c3,$99,$99,$c3,$ff
	!byte $c3,$99,$99,$c1,$f9,$99,$c3,$ff
	!byte $ff,$ff,$e7,$ff,$ff,$e7,$ff,$ff
	!byte $ff,$ff,$e7,$ff,$ff,$e7,$e7,$cf
	!byte $f1,$e7,$cf,$9f,$cf,$e7,$f1,$ff
	!byte $ff,$ff,$81,$ff,$81,$ff,$ff,$ff
	!byte $8f,$e7,$f3,$f9,$f3,$e7,$8f,$ff
	!byte $c3,$99,$f9,$f3,$e7,$ff,$e7,$ff
	!byte $ff,$ff,$ff,$00,$00,$ff,$ff,$ff
	!byte $f7,$e3,$c1,$80,$80,$e3,$c1,$ff
	!byte $e7,$e7,$e7,$e7,$e7,$e7,$e7,$e7
	!byte $ff,$ff,$ff,$00,$00,$ff,$ff,$ff
	!byte $ff,$ff,$00,$00,$ff,$ff,$ff,$ff
	!byte $ff,$00,$00,$ff,$ff,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$ff,$00,$00,$ff,$ff
	!byte $cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf
	!byte $f3,$f3,$f3,$f3,$f3,$f3,$f3,$f3
	!byte $ff,$ff,$ff,$1f,$0f,$c7,$e7,$e7
	!byte $e7,$e7,$e3,$f0,$f8,$ff,$ff,$ff
	!byte $e7,$e7,$c7,$0f,$1f,$ff,$ff,$ff
	!byte $3f,$3f,$3f,$3f,$3f,$3f,$00,$00
	!byte $3f,$1f,$8f,$c7,$e3,$f1,$f8,$fc
	!byte $fc,$f8,$f1,$e3,$c7,$8f,$1f,$3f
	!byte $00,$00,$3f,$3f,$3f,$3f,$3f,$3f
	!byte $00,$00,$fc,$fc,$fc,$fc,$fc,$fc
	!byte $ff,$c3,$81,$81,$81,$81,$c3,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$00,$00,$ff
	!byte $c9,$80,$80,$80,$c1,$e3,$f7,$ff
	!byte $9f,$9f,$9f,$9f,$9f,$9f,$9f,$9f
	!byte $ff,$ff,$ff,$f8,$f0,$e3,$e7,$e7
	!byte $3c,$18,$81,$c3,$c3,$81,$18,$3c
	!byte $ff,$c3,$81,$99,$99,$81,$c3,$ff
	!byte $e7,$e7,$99,$99,$e7,$e7,$c3,$ff
	!byte $f9,$f9,$f9,$f9,$f9,$f9,$f9,$f9
	!byte $f7,$e3,$c1,$80,$c1,$e3,$f7,$ff
	!byte $e7,$e7,$e7,$00,$00,$e7,$e7,$e7
	!byte $3f,$3f,$cf,$cf,$3f,$3f,$cf,$cf
	!byte $e7,$e7,$e7,$e7,$e7,$e7,$e7,$e7
	!byte $ff,$ff,$fc,$c1,$89,$c9,$c9,$ff
	!byte $00,$80,$c0,$e0,$f0,$f8,$fc,$fe
	!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	!byte $ff,$ff,$ff,$ff,$00,$00,$00,$00
	!byte $00,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$00
	!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
	!byte $33,$33,$cc,$cc,$33,$33,$cc,$cc
	!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
	!byte $ff,$ff,$ff,$ff,$33,$33,$cc,$cc
	!byte $00,$01,$03,$07,$0f,$1f,$3f,$7f
	!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
	!byte $e7,$e7,$e7,$e0,$e0,$e7,$e7,$e7
	!byte $ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0
	!byte $e7,$e7,$e7,$e0,$e0,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$07,$07,$e7,$e7,$e7
	!byte $ff,$ff,$ff,$ff,$ff,$ff,$00,$00
	!byte $ff,$ff,$ff,$e0,$e0,$e7,$e7,$e7
	!byte $e7,$e7,$e7,$00,$00,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$00,$00,$e7,$e7,$e7
	!byte $e7,$e7,$e7,$07,$07,$e7,$e7,$e7
	!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
	!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f
	!byte $f8,$f8,$f8,$f8,$f8,$f8,$f8,$f8
	!byte $00,$00,$ff,$ff,$ff,$ff,$ff,$ff
	!byte $00,$00,$00,$ff,$ff,$ff,$ff,$ff
	!byte $ff,$ff,$ff,$ff,$ff,$00,$00,$00
	!byte $fc,$fc,$fc,$fc,$fc,$fc,$00,$00
	!byte $ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f
	!byte $f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff
	!byte $e7,$e7,$e7,$07,$07,$ff,$ff,$ff
	!byte $0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff
	!byte $0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0

	;Logo color data (we only need the first 297 bytes)
	*= $6000
	!binary "data/unifaun.col",297

	;Logo screen data (we only need the first 318 bytes)
	*= $63eb
	!binary "data/unifaun.scr",318

	;Text color data (40 cols * 13 rows = 520 bytes)
	*= $8000
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

	;Horizontal lines color cycle data (40 bytes)
	*= $8281
	!byte $0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0f,$0f,$05,$05,$07,$07,$03,$03,$01,$01,$01,$01,$01,$01,$01,$01,$03,$03,$07,$07,$05,$05,$0f,$0f,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0b

	;Cursor color cycle data (20 bytes)
	*= $82aa
	!byte $00,$00,$0b,$0b,$0c,$0c,$0f,$0f,$03,$01,$01,$03,$0f,$0f,$0c,$0c,$0b,$0b,$00,$00

	;Text data (40 cols * 13 rows = 520 bytes per screen)
	*= $9000
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
	!scr "                                        "
	!scr "                                        "
	!scr "                                        "
	!scr "         code and graphics by           "
	!scr "        clifford 'randy' carnmo         "
	!scr "                                        "
	!scr "               music by                 "
	!scr "       stellan 'dane' andersson         "
	!scr "                                        "
	!scr "                                        "
	!scr "                                        "
	!scr "                                        "

	;Control byte
	!byte $ff