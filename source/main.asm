	;$64 = character index
	;$65 = row index
	;$66 = cursor phase index
	;$67 = delay index

	;$80 = color memory pointer 
	;$82 = screen memory pointer
	;$84 = text color pointer
	;$86 = text pointer

	*= $0801

	jsr $e544

	lda #$7f
	sta $67

	lda #$00
	sta $d020
	sta $d021

	jsr $1000
	
	jsr setpointers

logo:
	ldx #$00
-
	lda $3000,x
	sta $d800,x

	lda $33eb,x
	sta $0400,x

	inx
	cpx #$ff
	bne -

	ldx #$00
-
	lda $3100,x
	sta $d900,x

	lda $34eb,x
	sta $0500,x

	inx
	cpx #$3e
	bne -

horizontallines:
	ldx #$00
-
	lda #$63
	sta $0568,x
	sta $0568 + 40 * 15,x

	inx
	cpx #40
	bne -

main:
	sei

	lda #$35
	sta $01

	lda #$7f
	sta $dc0d
	sta $dd0d

	lda #$01
	sta $d01a

	lda #$00
	sta $d012

	lda #<gfxirq 
	sta $fffe
	lda #>gfxirq
	sta $ffff

	cli

	jmp *

gfxirq:
	asl $d019

	jsr $1003

	lda #$3b
	sta $d011

	lda #$18
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

colorcycle:
	lda $8281
	sta $8281 + 40

	ldx #$00
-	lda $8281 + 1,x
	sta $8281,x
	sta $d968,x
	sta $d968 + 40 * 15,x

	inx
	cpx #40
	bne -
	rts

textwriter:
	lda $66
	cmp #19
	beq resetcursorcolor

	inc $66

	lda $67
	cmp #$00
	beq dotext
	bcs pause
	rts

pause:
	jsr writecursor

	dec $67
	
	lda $67
	cmp #$00
	beq clearscreen
	rts

dotext:
	jsr writecharacter
	jsr writecursor
	rts

resetcursorcolor:
	lda #$00
	sta $66
	rts

writecursor:
	ldx $66
	ldy $64
	lda $82aa,x
	sta ($80),y

	lda #$e0
	sta ($82),y
	rts

writecharacter:
	ldy $64
	lda ($84),y
	sta ($80),y

	lda ($86),y
	cmp #$ff
	beq restart

	sta ($82),y

	cpy #39
	beq nextrow

	inc $64
	rts

pushoffsets:
	clc

	lda $80
	adc #40
	sta $80
	lda $81
	adc #$00
	sta $81

	lda $84
	adc #40
	sta $84
	lda $85
	adc #$00
	sta $85

	lda $82
	adc #40
	sta $82
	lda $83
	adc #$00
	sta $83
	rts

pushoffsets2:
	clc

	lda $86
	adc #40
	sta $86
	lda $87
	adc #$00
	sta $87
	rts

nextrow:
	jsr pushoffsets2

	lda $65
	cmp #12
	beq nextscreen

	jsr pushoffsets

	lda #$00
	sta $64

	inc $65
	rts

nextscreen:
	lda #$ff
	sta $67
	rts

clearscreen:
	lda #$20
	ldx #$00
-
	sta $0590,x
	sta $0590 + 255,x

	inx
	cpx #$ff
	bne -

	ldx #$00
-
	sta $0590 + 255 * 2,x

	inx
	cpx #50
	bne -

	jsr clearindices
	jsr setcolorpointer
	jsr settextcolorpointer
	jsr setscreenpointer
	rts

restart:
	jsr setpointers
	rts

setpointers:
	jsr clearindices
	jsr setcolorpointer
	jsr settextcolorpointer
	jsr setscreenpointer
	jsr settextpointer
	rts

settextpointer:
	lda #$00
	sta $86
	lda #$83
	sta $87
	rts

setscreenpointer:
	lda #$90
	sta $82
	lda #$05
	sta $83
	rts

setcolorpointer:
	lda #$90
	sta $80
	lda #$d9
	sta $81
	rts

settextcolorpointer:
	lda #$00
	sta $84
	lda #$80
	sta $85
	rts

clearindices:
	lda #$00
	sta $66
	sta $64
	sta $65
	rts

	*= $1000
	!binary "data/Goldie.sid",,$7e

	*= $2000
	!binary "data/unifaun.map",2540

	*= $3000
	!binary "data/unifaun.col",297

	*= $33eb
	!binary "data/unifaun.scr",318

	*= $3800
	!source "data/charset.dat"
	
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

	*= $8281
	!byte $0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0f,$0f,$05,$05,$07,$07,$03,$03,$01,$01,$01,$01,$01,$01,$01,$01,$03,$03,$07,$07,$05,$05,$0f,$0f,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0b

	*= $82aa
	!byte $00,$00,$0b,$0b,$0c,$0c,$0f,$0f,$03,$01,$01,$03,$0f,$0f,$0c,$0c,$0b,$0b,$00,$00

	*= $8300
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