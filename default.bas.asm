 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
  if bankswitch == 64
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif

 ifconst bankswitch_hotspot
 if bankswitch_hotspot = $083F ; 0840 bankswitching hotspot
   .byte 0 ; stop unexpected bankswitches
 endif
 endif
start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifnconst multisprite
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 endif
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
     ; This is a 2-line kernel!
     ifnconst vertical_reflect
kernel
     endif
     sta WSYNC
     lda #255
     sta TIM64T

     lda #1
     sta VDELBL
     sta VDELP0
     ldx ballheight
     inx
     inx
     stx temp4
     lda player1y
     sta temp3

     ifconst shakescreen
         jsr doshakescreen
     else
         ldx missile0height
         inx
     endif

     inx
     stx stack1

     lda bally
     sta stack2

     lda player0y
     ldx #0
     sta WSYNC
     stx GRP0
     stx GRP1
     stx PF1L
     stx PF2
     stx CXCLR
     ifconst readpaddle
         stx paddle
     else
         sleep 3
     endif

     sta temp2,x

     ;store these so they can be retrieved later
     ifnconst pfres
         ldx #128-44+(4-pfwidth)*12
     else
         ldx #132-pfres*pfwidth
     endif

     dec player0y

     lda missile0y
     sta temp5
     lda missile1y
     sta temp6

     lda playfieldpos
     sta temp1
     
     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif
     clc
     sbc playfieldpos
     sta playfieldpos
     jmp .startkernel

.skipDrawP0
     lda #0
     tay
     jmp .continueP0

.skipDrawP1
     lda #0
     tay
     jmp .continueP1

.kerloop     ; enter at cycle 59??

continuekernel
     sleep 2
continuekernel2
     lda ballheight
     
     ifconst pfres
         ldy playfield+pfres*pfwidth-132,x
         sty PF1L ;3
         ldy playfield+pfres*pfwidth-131-pfadjust,x
         sty PF2L ;3
         ldy playfield+pfres*pfwidth-129,x
         sty PF1R ; 3 too early?
         ldy playfield+pfres*pfwidth-130-pfadjust,x
         sty PF2R ;3
     else
         ldy playfield-48+pfwidth*12+44-128,x
         sty PF1L ;3
         ldy playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sty PF2L ;3
         ldy playfield-48+pfwidth*12+47-128,x ;4
         sty PF1R ; 3 too early?
         ldy playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sty PF2R ;3
     endif

     ; should be playfield+$38 for width=2

     dcp bally
     rol
     rol
     ; rol
     ; rol
goback
     sta ENABL 
.startkernel
     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawP1 ;2
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continueP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         lda (player1color),y
         sta COLUP1
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif

     ifconst pfres
         lda playfield+pfres*pfwidth-132,x 
         sta PF1L ;3
         lda playfield+pfres*pfwidth-131-pfadjust,x 
         sta PF2L ;3
         lda playfield+pfres*pfwidth-129,x 
         sta PF1R ; 3 too early?
         lda playfield+pfres*pfwidth-130-pfadjust,x 
         sta PF2R ;3
     else
         lda playfield-48+pfwidth*12+44-128,x ;4
         sta PF1L ;3
         lda playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sta PF2L ;3
         lda playfield-48+pfwidth*12+47-128,x ;4
         sta PF1R ; 3 too early?
         lda playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sta PF2R ;3
     endif 
     ; sleep 3

     lda player0height
     dcp player0y
     bcc .skipDrawP0
     ldy player0y
     lda (player0pointer),y
.continueP0
     sta GRP0

     ifnconst no_blank_lines
         ifnconst playercolors
             lda missile0height ;3
             dcp missile0y ;5
             sbc stack1
             sta ENAM0 ;3
         else
             lda (player0color),y
             sta player0colorstore
             sleep 6
         endif
         dec temp1
         bne continuekernel
     else
         dec temp1
         beq altkernel2
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle
             inc paddle
             jmp continuekernel2
noreadpaddle
             sleep 2
             jmp continuekernel
         else
             ifnconst playercolors 
                 ifconst PFcolors
                     txa
                     tay
                     lda (pfcolortable),y
                     ifnconst backgroundchange
                         sta COLUPF
                     else
                         sta COLUBK
                     endif
                     jmp continuekernel
                 else
                     ifconst kernelmacrodef
                         kernelmacro
                     else
                         sleep 12
                     endif
                 endif
             else
                 lda (player0color),y
                 sta player0colorstore
                 sleep 4
             endif
             jmp continuekernel
         endif
altkernel2
         txa
         ifnconst vertical_reflect
             sbx #256-pfwidth
         else
             sbx #256-pfwidth/2
         endif
         bmi lastkernelline
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
         jmp continuekernel
     endif

altkernel

     ifconst PFmaskvalue
         lda #PFmaskvalue
     else
         lda #0
     endif
     sta PF1L
     sta PF2


     ;sleep 3

     ;28 cycles to fix things
     ;minus 11=17

     ; lax temp4
     ; clc
     txa
     ifnconst vertical_reflect
         sbx #256-pfwidth
     else
         sbx #256-pfwidth/2
     endif

     bmi lastkernelline

     ifconst PFcolorandheight
         ifconst pfres
             ldy playfieldcolorandheight-131+pfres*pfwidth,x
         else
             ldy playfieldcolorandheight-87,x
         endif
         ifnconst backgroundchange
             sty COLUPF
         else
             sty COLUBK
         endif
         ifconst pfres
             lda playfieldcolorandheight-132+pfres*pfwidth,x
         else
             lda playfieldcolorandheight-88,x
         endif
         sta.w temp1
     endif
     ifconst PFheights
         lsr
         lsr
         tay
         lda (pfheighttable),y
         sta.w temp1
     endif
     ifconst PFcolors
         tay
         lda (pfcolortable),y
         ifnconst backgroundchange
             sta COLUPF
         else
             sta COLUBK
         endif
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
     endif
     ifnconst PFcolorandheight
         ifnconst PFcolors
             ifnconst PFheights
                 ifnconst no_blank_lines
                     ; read paddle 0
                     ; lo-res paddle read
                     ; bit INPT0
                     ; bmi paddleskipread
                     ; inc paddle0
                     ;donepaddleskip
                     sleep 10
                     ifconst pfrowheight
                         lda #pfrowheight
                     else
                         ifnconst pfres
                             lda #8
                         else
                             lda #(96/pfres) ; try to come close to the real size
                         endif
                     endif
                     sta temp1
                 endif
             endif
         endif
     endif
     

     lda ballheight
     dcp bally
     sbc temp4


     jmp goback


     ifnconst no_blank_lines
lastkernelline
         ifnconst PFcolors
             sleep 10
         else
             ldy #124
             lda (pfcolortable),y
             sta COLUPF
         endif

         ifconst PFheights
             ldx #1
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 3
             sleep 2 ; REVENG - this was over 1 cycle
         endif

         jmp enterlastkernel

     else
lastkernelline
         
         ifconst PFheights
             ldx #1
             ;sleep 5
             sleep 4 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         endif

         cpx #0
         bne .enterfromNBL
         jmp no_blank_lines_bailout
     endif

     if ((<*)>$d5)
         align 256
     endif
     ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
     sleep 2
     lda #0
     jmp .continuelastP1

.endkerloop     ; enter at cycle 59??
     
     nop

.enterfromNBL
     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

enterlastkernel
     lda ballheight

     ; tya
     dcp bally
     ; sleep 4

     ; sbc stack3
     rol
     rol
     sta ENABL 

     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawlastP1
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continuelastP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
     else
         lda (player1color),y
         sta COLUP1
     endif

     dex
     ;dec temp4 ; might try putting this above PF writes
     beq endkernel


     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

     ifnconst player1colors
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif
     
     lda.w player0height
     dcp player0y
     bcc .skipDrawlastP0
     ldy player0y
     lda (player0pointer),y
.continuelastP0
     sta GRP0



     ifnconst no_blank_lines
         lda missile0height ;3
         dcp missile0y ;5
         sbc stack1
         sta ENAM0 ;3
         jmp .endkerloop
     else
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle2
             inc paddle
             jmp .endkerloop
noreadpaddle2
             sleep 4
             jmp .endkerloop
         else ; no_blank_lines and no paddle reading
             pla
             pha ; 14 cycles in 4 bytes
             pla
             pha
             ; sleep 14
             jmp .endkerloop
         endif
     endif


     ; ifconst donepaddleskip
         ;paddleskipread
         ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
         ; plus we get a lo-res paddle read
         ; bmi donepaddleskip
     ; endif

.skipDrawlastP0
     sleep 2
     lda #0
     jmp .continuelastP0

     ifconst no_blank_lines
no_blank_lines_bailout
         ldx #0
     endif

endkernel
     ; 6 digit score routine
     stx PF1
     stx PF2
     stx PF0
     clc

     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif

     sbc playfieldpos
     sta playfieldpos
     txa

     ifconst shakescreen
         bit shakescreen
         bmi noshakescreen2
         ldx #$3D
noshakescreen2
     endif

     sta WSYNC,x

     ; STA WSYNC ;first one, need one more
     sta REFP0
     sta REFP1
     STA GRP0
     STA GRP1
     ; STA PF1
     ; STA PF2
     sta HMCLR
     sta ENAM0
     sta ENAM1
     sta ENABL

     lda temp2 ;restore variables that were obliterated by kernel
     sta player0y
     lda temp3
     sta player1y
     ifnconst player1colors
         lda temp6
         sta missile1y
     endif
     ifnconst playercolors
         ifnconst readpaddle
             lda temp5
             sta missile0y
         endif
     endif
     lda stack2
     sta bally

     ; REVENG - strangely, this isn't required any more. might have
     ; resulted from the no_blank_lines score bounce fix
     ;ifconst no_blank_lines
         ;sta WSYNC
     ;endif

     lda INTIM
     clc
     ifnconst vblank_time
         adc #43+12+87
     else
         adc #vblank_time+12+87

     endif
     ; sta WSYNC
     sta TIM64T

     ifconst minikernel
         jsr minikernel
     endif

     ; now reassign temp vars for score pointers

     ; score pointers contain:
     ; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
     ; swap lo2->temp1
     ; swap lo4->temp3
     ; swap lo6->temp5
     ifnconst noscore
         lda scorepointers+1
         ; ldy temp1
         sta temp1
         ; sty scorepointers+1

         lda scorepointers+3
         ; ldy temp3
         sta temp3
         ; sty scorepointers+3


         sta HMCLR
         tsx
         stx stack1 
         ldx #$E0
         stx HMP0

         LDA scorecolor 
         STA COLUP0
         STA COLUP1
         ifconst scorefade
             STA stack2
         endif
         ifconst pfscore
             lda pfscorecolor
             sta COLUPF
         endif
         sta WSYNC
         ldx #0
         STx GRP0
         STx GRP1 ; seems to be needed because of vdel

         lda scorepointers+5
         ; ldy temp5
         sta temp5,x
         ; sty scorepointers+5
         lda #>scoretable
         sta scorepointers+1
         sta scorepointers+3
         sta scorepointers+5
         sta temp2
         sta temp4
         sta temp6
         LDY #7
         STY VDELP0
         STA RESP0
         STA RESP1


         LDA #$03
         STA NUSIZ0
         STA NUSIZ1
         STA VDELP1
         LDA #$F0
         STA HMP1
         lda (scorepointers),y
         sta GRP0
         STA HMOVE ; cycle 73 ?
         jmp beginscore


         if ((<*)>$d4)
             align 256 ; kludge that potentially wastes space! should be fixed!
         endif

loop2
         lda (scorepointers),y ;+5 68 204
         sta GRP0 ;+3 71 213 D1 -- -- --
         ifconst pfscore
             lda.w pfscore1
             sta PF1
         else
             ifconst scorefade
                 sleep 2
                 dec stack2 ; decrement the temporary scorecolor
             else
                 sleep 7
             endif
         endif
         ; cycle 0
beginscore
         lda (scorepointers+$8),y ;+5 5 15
         sta GRP1 ;+3 8 24 D1 D1 D2 --
         lda (scorepointers+$6),y ;+5 13 39
         sta GRP0 ;+3 16 48 D3 D1 D2 D2
         lax (scorepointers+$2),y ;+5 29 87
         txs
         lax (scorepointers+$4),y ;+5 36 108
         ifconst scorefade
             lda stack2
         else
             sleep 3
         endif

         ifconst pfscore
             lda pfscore2
             sta PF1
         else
             ifconst scorefade
                 sta COLUP0
                 sta COLUP1
             else
                 sleep 6
             endif
         endif

         lda (scorepointers+$A),y ;+5 21 63
         stx GRP1 ;+3 44 132 D3 D3 D4 D2!
         tsx
         stx GRP0 ;+3 47 141 D5 D3! D4 D4
         sta GRP1 ;+3 50 150 D5 D5 D6 D4!
         sty GRP0 ;+3 53 159 D4* D5! D6 D6
         dey
         bpl loop2 ;+2 60 180

         ldx stack1 
         txs
         ; lda scorepointers+1
         ldy temp1
         ; sta temp1
         sty scorepointers+1

         LDA #0 
         sta PF1
         STA GRP0
         STA GRP1
         STA VDELP0
         STA VDELP1;do we need these
         STA NUSIZ0
         STA NUSIZ1

         ; lda scorepointers+3
         ldy temp3
         ; sta temp3
         sty scorepointers+3

         ; lda scorepointers+5
         ldy temp5
         ; sta temp5
         sty scorepointers+5
     endif ;noscore
     LDA #%11000010
     sta WSYNC
     STA VBLANK
     RETURN

     ifconst shakescreen
doshakescreen
         bit shakescreen
         bmi noshakescreen
         sta WSYNC
noshakescreen
         ldx missile0height
         inx
         rts
     endif

; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*pfwidth-1
 else
 ldx #47-(4-pfwidth)*12 ; will this work?
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 if pfwidth=4
  asl ; multiply y pos by 4
 endif ; else multiply by 2
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 if pfwidth=4
   asl ; multiply by 4
 endif ; else multiply by 2
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 if pfwidth=4
   iny
   iny
 endif
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 ifnconst pfcenter
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 endif
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
     ifconst debugscore
         ldx #14
         lda INTIM ; display # cycles left in the score

         ifconst mincycles
             lda mincycles 
             cmp INTIM
             lda mincycles
             bcc nochange
             lda INTIM
             sta mincycles
nochange
         endif

         ; cmp #$2B
         ; bcs no_cycles_left
         bmi cycles_left
         ldx #64
         eor #$ff ;make negative
cycles_left
         stx scorecolor
         and #$7f ; clear sign bit
         tax
         lda scorebcd,x
         sta score+2
         lda scorebcd1,x
         sta score+1
         jmp done_debugscore 
scorebcd
         .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
         .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
         .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
         .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
         .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
         .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
         .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
         .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
     endif

     ifconst debugcycles
         lda INTIM ; if we go over, it mucks up the background color
         ; cmp #$2B
         ; BCC overscan
         bmi overscan
         sta COLUBK
         bcs doneoverscan
     endif

overscan
     ifconst interlaced
         PHP
         PLA 
         EOR #4 ; flip interrupt bit
         PHA
         PLP
         AND #4 ; isolate the interrupt bit
         TAX ; save it for later
     endif

overscanloop
     lda INTIM ;wait for sync
     bmi overscanloop
doneoverscan

     ;do VSYNC

     ifconst interlaced
         CPX #4
         BNE oddframevsync
     endif

     lda #2
     sta WSYNC
     sta VSYNC
     STA WSYNC
     STA WSYNC
     lsr
     STA WSYNC
     STA VSYNC
     sta VBLANK
     ifnconst overscan_time
         lda #37+128
     else
         lda #overscan_time+128
     endif
     sta TIM64T

     ifconst interlaced
         jmp postsync 

oddframevsync
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #2
         sta VSYNC
         sta WSYNC
         sta WSYNC
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #0
         sta VSYNC
         sta VBLANK
         ifnconst overscan_time
             lda #37+128
         else
             lda #overscan_time+128
         endif
         sta TIM64T

postsync
     endif

     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop
             lda player0x,x
             sec
             sbc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop
         endif
     endif
     if ((<*)>$e9)&&((<*)<$fa)
         repeat ($fa-(<*))
         nop
         repend
     endif
     sta WSYNC
     ldx #4
     SLEEP 3
HorPosLoop     ; 5
     lda player0x,X ;+4 9
     sec ;+2 11
DivideLoop
     sbc #15
     bcs DivideLoop;+4 15
     sta temp1,X ;+4 19
     sta RESP0,X ;+4 23
     sta WSYNC
     dex
     bpl HorPosLoop;+5 5
     ; 4

     ldx #4
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 18

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 32

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 46

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 60

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 74

     sta WSYNC
     
     sta HMOVE ;+3 3


     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop2
             lda player0x,x
             clc
             adc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop2
         endif
     endif




     ;set score pointers
     lax score+2
     jsr scorepointerset
     sty scorepointers+5
     stx scorepointers+2
     lax score+1
     jsr scorepointerset
     sty scorepointers+4
     stx scorepointers+1
     lax score
     jsr scorepointerset
     sty scorepointers+3
     stx scorepointers

vblk
     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif
vblk2
     LDA INTIM
     bmi vblk2
     jmp kernel
     

     .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
     .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
     and #$0F
     asl
     asl
     asl
     adc #<scoretable
     tay 
     txa
     ; and #$F0
     ; lsr
     asr #$F0
     adc #<scoretable
     tax
     rts
game
.
 ; 

.
 ; 

.
 ; 

.L00 ;  set smartbranching on

.L01 ;  set kernel_options player1colors playercolors pfcolors

.L02 ;  rem *  Cost: loss of missile1 and missile0.

.
 ; 

.
 ; 

.
 ; 

.L03 ;  const COLOR_SCORE_RED  =  $4A

.L04 ;  const COLOR_SCORE_BLUE  =  $8A

.L05 ;  const COLOR_SCORE_GREEN  =  $CA

.L06 ;  const COLOR_SCORE_YELLOW  =  $1A

.L07 ;  const COLOR_SCORE_GREY  =  $0A

.
 ; 

.L08 ;  const scorefade  =  1

.L09 ;  scorecolor  =  COLOR_SCORE_BLUE

	LDA #COLOR_SCORE_BLUE
	STA scorecolor
.
 ; 

.L010 ;  const ONE_COPY  =  0

.L011 ;  const TWO_COPIES_CLOSE  =  1

.L012 ;  const TWO_COPIES_MED  =  2

.L013 ;  const THREE_COPIES_CLOSE  =  3

.L014 ;  const TWO_COPIES_WIDE  =  4

.L015 ;  const DOUBLE_SIZE  =  5

.L016 ;  const THREE_COPIES_MED  =  6

.L017 ;  const QUAD_SIZE  =  7

.
 ; 

.L018 ;  const PF_NO_REFLECT  =  $00

.L019 ;  const PF_REFLECT  =  $01

.L020 ;  const PF_SCORE  =  $02

.L021 ;  const PF_PRIORITY  =  $04

.L022 ;  const BALL_WIDTH_8 =  $31

.L023 ;  const BALL_WIDTH_4  =  $21

.L024 ;  const BALL_WIDTH_2  =  $11

.L025 ;  const BALL_WIDTH_1  =  $01

.
 ; 

.L026 ;  const HERO_X_POS_DEFAULT  =  80

.L027 ;  const HERO_Y_POS_DEFAULT  =  50

.L028 ;  const ENEMY_X_POS_DEFAULT  =  63  :  rem 136 is right side

.L029 ;  const ENEMY_Y_POS_DEFAULT  =  15

.L030 ;  const ENEMY_Y_POS_HIDE_SCREEN  =  255

.L031 ;  const ENEMY_X_POS_RIGHT_SPRITE  =  32

.L032 ;  const ENEMY_X_POS_CENTER_SPRITE  =  16

.L033 ;  const BULLET_X_POS_DEFAULT  =  0

.L034 ;  const BULLET_Y_POS_DEFAULT  =  0

.L035 ;  const BULLET_SPEED  =  3

.
 ; 

.L036 ;  const HALF_SECOND_MASK  =  $1F

.L037 ;  const NUM_OF_ENEMY_PATTERNS  =  7

.
 ; 

.L038 ;  const _Edge_Top  =  9

.L039 ;  const _Edge_Bottom  =  88

.L040 ;  const _Edge_Left  =  1

.L041 ;  const _Edge_Right  =  153

.
 ; 

.L042 ;  const COLOR_DIRT  =  $f5

.L043 ;  const COLOR_GRASS  =  $C0

.L044 ;  const COLOR_STONE  =  $02

.
 ; 

.L045 ;  dim enemyPattern  =  a

.L046 ;  dim frameCounter  =  b

.L047 ;  dim heroX  =  c

.L048 ;  dim heroY  =  d

.L049 ;  dim bulletX  =  e

.L050 ;  dim bulletY  =  f

.L051 ;  dim enemyX  =  g

.L052 ;  dim enemyY  =  h

.L053 ;  dim soldier  =  i

.L054 ;  dim facing  =  j

;.heroNorth.facing{0}.
.L055 ;  def heroNorth = facing{0}

.L056 ;  heroNorth  =  1

	LDA facing
	ORA #1
	STA facing
;.heroEast.facing{1}.
.L057 ;  def heroEast = facing{1}

.L058 ;  heroEast  =  0

	LDA facing
	AND #253
	STA facing
;.heroSouth.facing{2}.
.L059 ;  def heroSouth = facing{2}

.L060 ;  heroSouth  =  0

	LDA facing
	AND #251
	STA facing
;.heroWest.facing{3}.
.L061 ;  def heroWest = facing{3}

.L062 ;  heroWest  =  0

	LDA facing
	AND #247
	STA facing
;.bulletNorth.facing{4}.
.L063 ;  def bulletNorth = facing{4}

.L064 ;  bulletNorth  =  0

	LDA facing
	AND #239
	STA facing
;.bulletEast.facing{5}.
.L065 ;  def bulletEast = facing{5}

.L066 ;  bulletEast  =  0

	LDA facing
	AND #223
	STA facing
;.bulletSouth.facing{6}.
.L067 ;  def bulletSouth = facing{6}

.L068 ;  bulletSouth  =  0

	LDA facing
	AND #191
	STA facing
;.bulletWest.facing{7}.
.L069 ;  def bulletWest = facing{7}

.L070 ;  bulletWest  =  0

	LDA facing
	AND #127
	STA facing
.
 ; 

.L071 ;  dim game_flags  =  w

.
 ; 

.L072 ;  dim mytemp1  =  x

.L073 ;  dim mytemp2  =  y

.L074 ;  dim mytemp3  =  z

.
 ; 

.L075 ;  heroX  =  HERO_X_POS_DEFAULT

	LDA #HERO_X_POS_DEFAULT
	STA heroX
.L076 ;  heroY  =  HERO_Y_POS_DEFAULT

	LDA #HERO_Y_POS_DEFAULT
	STA heroY
.L077 ;  bulletX  =  BULLET_X_POS_DEFAULT

	LDA #BULLET_X_POS_DEFAULT
	STA bulletX
.L078 ;  bulletY  =  BULLET_Y_POS_DEFAULT

	LDA #BULLET_Y_POS_DEFAULT
	STA bulletY
.L079 ;  enemyX  =  ENEMY_X_POS_DEFAULT

	LDA #ENEMY_X_POS_DEFAULT
	STA enemyX
.L080 ;  enemyY  =  ENEMY_Y_POS_DEFAULT

	LDA #ENEMY_Y_POS_DEFAULT
	STA enemyY
.L081 ;  enemyPattern  =  0

	LDA #0
	STA enemyPattern
.
 ; 

.L082 ;  gosub drawLevel1

 jsr .drawLevel1

.
 ; 

.
 ; 

.
 ; 

.main
 ; main

.
 ; 

.doEnemy
 ; doEnemy

.L083 ;  rem  Update enemy pattern approximately every half second,

.L084 ;  rem  and scroll through enemy pattern sequence continually...

.L085 ;  frameCounter  =  frameCounter  +  1

	INC frameCounter
.L086 ;  temp1  =  frameCounter  &  HALF_SECOND_MASK

	LDA frameCounter
	AND #HALF_SECOND_MASK
	STA temp1
.L087 ;  if temp1  =  0 then enemyPattern  =  enemyPattern  +  1

	LDA temp1
	CMP #0
     BNE .skipL087
.condpart0
	INC enemyPattern
.skipL087
.L088 ;  if enemyPattern  >  NUM_OF_ENEMY_PATTERNS then enemyPattern  =  0

	LDA #NUM_OF_ENEMY_PATTERNS
	CMP enemyPattern
     BCS .skipL088
.condpart1
	LDA #0
	STA enemyPattern
.skipL088
.L089 ;  rem enemyPattern=7

.
 ; 

.L090 ;  rem soldier uses 3 bits. each one represents a soldier. 1 for alive, 0 for dead

.L091 ;  rem 0      hide the soldiers offscreen

.L092 ;  rem 1   X  NUSIZ1 = ONE_COPY, player1x = enemyX + ENEMY_X_POS_RIGHT_SPRITE

.L093 ;  rem 2  X   NUSIZ1 = ONE_COPY, player1x = enemyX + ENEMY_X_POS_CENTER_SPRITE

.L094 ;  rem 3  XX  NUSIZ1 = TWO_COPIES_CLOSE, player1x = enemyX + ENEMY_X_POS_CENTER_SPRITE

.L095 ;  rem 4 X    NUSIZ1 = ONE_COPY

.L096 ;  rem 5 X X  NUSIZ1 = TWO_COPIES_MED

.L097 ;  rem 6 XX   NUSIZ1 = TWO_COPIES_CLOSE

.L098 ;  rem 7 XXX  NUSIZ1 = THREE_COPIES_CLOSE

.
 ; 

.L099 ;  if enemyPattern  =  0 then soldier{0} = 0  :  soldier{1} = 0  :  soldier{2} = 0  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #0
     BNE .skipL099
.condpart2
	LDA soldier
	AND #254
	STA soldier
	LDA soldier
	AND #253
	STA soldier
	LDA soldier
	AND #251
	STA soldier
 jmp .enemyPatternSet

.skipL099
.L0100 ;  if enemyPattern  =  1 then soldier{0} = 0  :  soldier{1} = 0  :  soldier{2} = 1  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #1
     BNE .skipL0100
.condpart3
	LDA soldier
	AND #254
	STA soldier
	LDA soldier
	AND #253
	STA soldier
	LDA soldier
	ORA #4
	STA soldier
 jmp .enemyPatternSet

.skipL0100
.L0101 ;  if enemyPattern  =  2 then soldier{0} = 0  :  soldier{1} = 1  :  soldier{2} = 0  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #2
     BNE .skipL0101
.condpart4
	LDA soldier
	AND #254
	STA soldier
	LDA soldier
	ORA #2
	STA soldier
	LDA soldier
	AND #251
	STA soldier
 jmp .enemyPatternSet

.skipL0101
.L0102 ;  if enemyPattern  =  3 then soldier{0} = 0  :  soldier{1} = 1  :  soldier{2} = 1  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #3
     BNE .skipL0102
.condpart5
	LDA soldier
	AND #254
	STA soldier
	LDA soldier
	ORA #2
	STA soldier
	LDA soldier
	ORA #4
	STA soldier
 jmp .enemyPatternSet

.skipL0102
.L0103 ;  if enemyPattern  =  4 then soldier{0} = 1  :  soldier{1} = 0  :  soldier{2} = 0  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #4
     BNE .skipL0103
.condpart6
	LDA soldier
	ORA #1
	STA soldier
	LDA soldier
	AND #253
	STA soldier
	LDA soldier
	AND #251
	STA soldier
 jmp .enemyPatternSet

.skipL0103
.L0104 ;  if enemyPattern  =  5 then soldier{0} = 1  :  soldier{1} = 0  :  soldier{2} = 1  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #5
     BNE .skipL0104
.condpart7
	LDA soldier
	ORA #1
	STA soldier
	LDA soldier
	AND #253
	STA soldier
	LDA soldier
	ORA #4
	STA soldier
 jmp .enemyPatternSet

.skipL0104
.L0105 ;  if enemyPattern  =  6 then soldier{0} = 1  :  soldier{1} = 1  :  soldier{2} = 0  :  goto enemyPatternSet

	LDA enemyPattern
	CMP #6
     BNE .skipL0105
.condpart8
	LDA soldier
	ORA #1
	STA soldier
	LDA soldier
	ORA #2
	STA soldier
	LDA soldier
	AND #251
	STA soldier
 jmp .enemyPatternSet

.skipL0105
.L0106 ;  if enemyPattern  =  7 then soldier{0} = 1  :  soldier{1} = 1  :  soldier{2} = 1

	LDA enemyPattern
	CMP #7
     BNE .skipL0106
.condpart9
	LDA soldier
	ORA #1
	STA soldier
	LDA soldier
	ORA #2
	STA soldier
	LDA soldier
	ORA #4
	STA soldier
.skipL0106
.enemyPatternSet
 ; enemyPatternSet

.
 ; 

.L0107 ;  rem position enemies

.L0108 ;  player1y  =  enemyY

	LDA enemyY
	STA player1y
.L0109 ;  if soldier{0} then goto leftSoldierFirst

	LDA soldier
	LSR
	BCC .skipL0109
.condpart10
 jmp .leftSoldierFirst

.skipL0109
.L0110 ;  if soldier{1} then goto centerSoldierFirst

	LDA soldier
	AND #2
	BEQ .skipL0110
.condpart11
 jmp .centerSoldierFirst

.skipL0110
.L0111 ;  if !soldier{2} then player1y  =  ENEMY_Y_POS_HIDE_SCREEN  :  goto soldiersDone

	LDA soldier
	AND #4
	BNE .skipL0111
.condpart12
	LDA #ENEMY_Y_POS_HIDE_SCREEN
	STA player1y
 jmp .soldiersDone

.skipL0111
.
 ; 

.L0112 ;  player1x  =  enemyX  +  ENEMY_X_POS_RIGHT_SPRITE

	LDA enemyX
	CLC
	ADC #ENEMY_X_POS_RIGHT_SPRITE
	STA player1x
.L0113 ;  NUSIZ1  =  ONE_COPY

	LDA #ONE_COPY
	STA NUSIZ1
.L0114 ;  goto soldiersDone

 jmp .soldiersDone

.
 ; 

.centerSoldierFirst
 ; centerSoldierFirst

.L0115 ;  player1x  =  enemyX  +  ENEMY_X_POS_CENTER_SPRITE

	LDA enemyX
	CLC
	ADC #ENEMY_X_POS_CENTER_SPRITE
	STA player1x
.L0116 ;  if soldier{2} then NUSIZ1  =  TWO_COPIES_CLOSE else NUSIZ1  =  ONE_COPY

	LDA soldier
	AND #4
	BEQ .skipL0116
.condpart13
	LDA #TWO_COPIES_CLOSE
	STA NUSIZ1
 jmp .skipelse0
.skipL0116
	LDA #ONE_COPY
	STA NUSIZ1
.skipelse0
.L0117 ;  goto soldiersDone

 jmp .soldiersDone

.
 ; 

.leftSoldierFirst
 ; leftSoldierFirst

.L0118 ;  player1x  =  enemyX

	LDA enemyX
	STA player1x
.L0119 ;  if soldier{1} then goto leftAndCenterSoldier

	LDA soldier
	AND #2
	BEQ .skipL0119
.condpart14
 jmp .leftAndCenterSoldier

.skipL0119
.L0120 ;  if soldier{2} then NUSIZ1  =  TWO_COPIES_MED else NUSIZ1  =  ONE_COPY

	LDA soldier
	AND #4
	BEQ .skipL0120
.condpart15
	LDA #TWO_COPIES_MED
	STA NUSIZ1
 jmp .skipelse1
.skipL0120
	LDA #ONE_COPY
	STA NUSIZ1
.skipelse1
.L0121 ;  goto soldiersDone

 jmp .soldiersDone

.
 ; 

.leftAndCenterSoldier
 ; leftAndCenterSoldier

.L0122 ;  if soldier{2} then NUSIZ1  =  THREE_COPIES_CLOSE else NUSIZ1  =  TWO_COPIES_CLOSE

	LDA soldier
	AND #4
	BEQ .skipL0122
.condpart16
	LDA #THREE_COPIES_CLOSE
	STA NUSIZ1
 jmp .skipelse2
.skipL0122
	LDA #TWO_COPIES_CLOSE
	STA NUSIZ1
.skipelse2
.
 ; 

.soldiersDone
 ; soldiersDone

.
 ; 

.L0123 ;  rem Process the input 

.
 ; 

.L0124 ;  if !joy0left then goto afterJoyLeft

 bit SWCHA
	BVC .skipL0124
.condpart17
 jmp .afterJoyLeft

.skipL0124
.L0125 ;  heroWest  =  1  :  heroEast  =  0  :  heroNorth  =  0  :  heroSouth  =  0

	LDA facing
	ORA #8
	STA facing
	LDA facing
	AND #253
	STA facing
	LDA facing
	AND #254
	STA facing
	LDA facing
	AND #251
	STA facing
.L0126 ;  if heroX  <=  _Edge_Left then goto afterJoyLeft

	LDA #_Edge_Left
	CMP heroX
     BCC .skipL0126
.condpart18
 jmp .afterJoyLeft

.skipL0126
.L0127 ;  mytemp1  =   ( heroX - 17 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #17
	lsr
	lsr
	STA mytemp1
.L0128 ;  mytemp2  =   ( heroY - 1 )  / 8

; complex statement detected
	LDA heroY
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA mytemp2
.L0129 ;  mytemp3  =   ( heroY - 8 )  / 8

; complex statement detected
	LDA heroY
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA mytemp3
.L0130 ;  if !pfread ( mytemp1 ,  mytemp2 )  then if !pfread ( mytemp1 ,  mytemp3 )  then heroX  =  heroX  -  1

	LDA mytemp1
	LDY mytemp2
 jsr pfread
	BEQ .skipL0130
.condpart19
	LDA mytemp1
	LDY mytemp3
 jsr pfread
	BEQ .skip19then
.condpart20
	DEC heroX
.skip19then
.skipL0130
.afterJoyLeft
 ; afterJoyLeft

.
 ; 

.L0131 ;  if !joy0right then goto afterJoyRight

 bit SWCHA
	BPL .skipL0131
.condpart21
 jmp .afterJoyRight

.skipL0131
.L0132 ;  heroEast  =  1  :  heroWest  =  0  :  heroNorth  =  0  :  heroSouth  =  0

	LDA facing
	ORA #2
	STA facing
	LDA facing
	AND #247
	STA facing
	LDA facing
	AND #254
	STA facing
	LDA facing
	AND #251
	STA facing
.L0133 ;  if heroX  >=  _Edge_Right then goto afterJoyRight

	LDA heroX
	CMP #_Edge_Right
     BCC .skipL0133
.condpart22
 jmp .afterJoyRight

.skipL0133
.L0134 ;  mytemp1  =   ( heroX - 10 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #10
	lsr
	lsr
	STA mytemp1
.L0135 ;  mytemp2  =   ( heroY - 1 )  / 8

; complex statement detected
	LDA heroY
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA mytemp2
.L0136 ;  mytemp3  =   ( heroY - 8 )  / 8

; complex statement detected
	LDA heroY
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA mytemp3
.L0137 ;  if !pfread ( mytemp1 ,  mytemp2 )  then if !pfread ( mytemp1 ,  mytemp3 )  then heroX  =  heroX  +  1

	LDA mytemp1
	LDY mytemp2
 jsr pfread
	BEQ .skipL0137
.condpart23
	LDA mytemp1
	LDY mytemp3
 jsr pfread
	BEQ .skip23then
.condpart24
	INC heroX
.skip23then
.skipL0137
.afterJoyRight
 ; afterJoyRight

.
 ; 

.L0138 ;  if !joy0down then goto afterJoyDown

 lda #$20
 bit SWCHA
	BEQ .skipL0138
.condpart25
 jmp .afterJoyDown

.skipL0138
.L0139 ;  heroSouth  =  1  :  heroNorth  =  0  :  heroEast  =  0  :  heroWest  =  0

	LDA facing
	ORA #4
	STA facing
	LDA facing
	AND #254
	STA facing
	LDA facing
	AND #253
	STA facing
	LDA facing
	AND #247
	STA facing
.L0140 ;  if heroY  >=  _Edge_Bottom then goto afterJoyDown

	LDA heroY
	CMP #_Edge_Bottom
     BCC .skipL0140
.condpart26
 jmp .afterJoyDown

.skipL0140
.L0141 ;  mytemp1  =   ( heroX - 16 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #16
	lsr
	lsr
	STA mytemp1
.L0142 ;  mytemp2  =  heroY / 8

	LDA heroY
	lsr
	lsr
	lsr
	STA mytemp2
.L0143 ;  mytemp3  =   ( heroX - 11 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #11
	lsr
	lsr
	STA mytemp3
.L0144 ;  if !pfread ( mytemp1 ,  mytemp2 )  then if !pfread ( mytemp3 ,  mytemp2 )  then heroY  =  heroY  +  1

	LDA mytemp1
	LDY mytemp2
 jsr pfread
	BEQ .skipL0144
.condpart27
	LDA mytemp3
	LDY mytemp2
 jsr pfread
	BEQ .skip27then
.condpart28
	INC heroY
.skip27then
.skipL0144
.afterJoyDown
 ; afterJoyDown

.
 ; 

.L0145 ;  if !joy0up then goto afterJoyUp

 lda #$10
 bit SWCHA
	BEQ .skipL0145
.condpart29
 jmp .afterJoyUp

.skipL0145
.L0146 ;  heroNorth  =  1  :  heroSouth  =  0  :  heroEast  =  0  :  heroWest  =  0

	LDA facing
	ORA #1
	STA facing
	LDA facing
	AND #251
	STA facing
	LDA facing
	AND #253
	STA facing
	LDA facing
	AND #247
	STA facing
.L0147 ;  if heroY  <=  _Edge_Top then goto afterJoyUp

	LDA #_Edge_Top
	CMP heroY
     BCC .skipL0147
.condpart30
 jmp .afterJoyUp

.skipL0147
.L0148 ;  mytemp1  =   ( heroX - 16 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #16
	lsr
	lsr
	STA mytemp1
.L0149 ;  mytemp2  =   ( heroY - 9 )  / 8

; complex statement detected
	LDA heroY
	SEC
	SBC #9
	lsr
	lsr
	lsr
	STA mytemp2
.L0150 ;  mytemp3  =   ( heroX - 11 )  / 4

; complex statement detected
	LDA heroX
	SEC
	SBC #11
	lsr
	lsr
	STA mytemp3
.L0151 ;  if !pfread ( mytemp1 ,  mytemp2 )  then if !pfread ( mytemp3 ,  mytemp2 )  then heroY  =  heroY  -  1

	LDA mytemp1
	LDY mytemp2
 jsr pfread
	BEQ .skipL0151
.condpart31
	LDA mytemp3
	LDY mytemp2
 jsr pfread
	BEQ .skip31then
.condpart32
	DEC heroY
.skip31then
.skipL0151
.afterJoyUp
 ; afterJoyUp

.
 ; 

.L0152 ;  rem if joy0up && enemyY > _Edge_Top then enemyY = enemyY - 1

.L0153 ;  rem if joy0down && enemyY < _Edge_Bottom then enemyY = enemyY + 1

.L0154 ;  rem if joy0right && enemyX < _Edge_Right then enemyX = enemyX + 1

.L0155 ;  rem if joy0left && enemyX > _Edge_Left then enemyX = enemyX - 1

.
 ; 

.L0156 ;  if !joy0fire then goto afterJoyFire

 bit INPT4
	BPL .skipL0156
.condpart33
 jmp .afterJoyFire

.skipL0156
.L0157 ;  if bulletX  =  0  &&  bulletY  =  0 then goto gonnaFire

	LDA bulletX
	CMP #0
     BNE .skipL0157
.condpart34
	LDA bulletY
	CMP #0
     BNE .skip34then
.condpart35
 jmp .gonnaFire

.skip34then
.skipL0157
.L0158 ;  goto afterJoyFire

 jmp .afterJoyFire

.gonnaFire
 ; gonnaFire

.L0159 ;  bulletX  =  heroX + 4  :  bulletY  =  heroY - 4

	LDA heroX
	CLC
	ADC #4
	STA bulletX
	LDA heroY
	SEC
	SBC #4
	STA bulletY
.L0160 ;  bulletNorth  =  heroNorth

	LDA facing
	AND #1
  PHP
	LDA facing
	AND #239
  PLP
	.byte $F0, $02
	ORA #16
	STA facing
.L0161 ;  bulletEast  =  heroEast

	LDA facing
	AND #2
  PHP
	LDA facing
	AND #223
  PLP
	.byte $F0, $02
	ORA #32
	STA facing
.L0162 ;  bulletSouth  =  heroSouth

	LDA facing
	AND #4
  PHP
	LDA facing
	AND #191
  PLP
	.byte $F0, $02
	ORA #64
	STA facing
.L0163 ;  bulletWest  =  heroWest

	LDA facing
	AND #8
  PHP
	LDA facing
	AND #127
  PLP
	.byte $F0, $02
	ORA #128
	STA facing
.afterJoyFire
 ; afterJoyFire

.
 ; 

.L0164 ;  rem Updating bullet position!

.L0165 ;  if bulletX  =  BULLET_X_POS_DEFAULT  &&  bulletY  =  BULLET_Y_POS_DEFAULT then goto bulletDone

	LDA bulletX
	CMP #BULLET_X_POS_DEFAULT
     BNE .skipL0165
.condpart36
	LDA bulletY
	CMP #BULLET_Y_POS_DEFAULT
     BNE .skip36then
.condpart37
 jmp .bulletDone

.skip36then
.skipL0165
.L0166 ;  if collision(ball,playfield) then goto bulletBad

	bit 	CXBLPF
	BPL .skipL0166
.condpart38
 jmp .bulletBad

.skipL0166
.L0167 ;  if bulletX  >=  3  &&  bulletY  >=  3  &&  bulletX  <=  _Edge_Right  &&  bulletY  <=  _Edge_Bottom then goto bulletOK

	LDA bulletX
	CMP #3
     BCC .skipL0167
.condpart39
	LDA bulletY
	CMP #3
     BCC .skip39then
.condpart40
	LDA #_Edge_Right
	CMP bulletX
     BCC .skip40then
.condpart41
	LDA #_Edge_Bottom
	CMP bulletY
     BCC .skip41then
.condpart42
 jmp .bulletOK

.skip41then
.skip40then
.skip39then
.skipL0167
.
 ; 

.bulletBad
 ; bulletBad

.L0168 ;  bulletX  =  BULLET_X_POS_DEFAULT

	LDA #BULLET_X_POS_DEFAULT
	STA bulletX
.L0169 ;  bulletY  =  BULLET_Y_POS_DEFAULT

	LDA #BULLET_Y_POS_DEFAULT
	STA bulletY
.L0170 ;  bulletNorth  =  0  :  bulletEast  =  0

	LDA facing
	AND #239
	STA facing
	LDA facing
	AND #223
	STA facing
.L0171 ;  bulletSouth  =  0  :  bulletWest  =  0

	LDA facing
	AND #191
	STA facing
	LDA facing
	AND #127
	STA facing
.L0172 ;  goto bulletDone

 jmp .bulletDone

.bulletOK
 ; bulletOK

.L0173 ;  if bulletNorth then bulletY  =  bulletY  -  BULLET_SPEED  :  goto bulletDone

	LDA facing
	AND #16
	BEQ .skipL0173
.condpart43
	LDA bulletY
	SEC
	SBC #BULLET_SPEED
	STA bulletY
 jmp .bulletDone

.skipL0173
.L0174 ;  if bulletEast then bulletX  =  bulletX  +  BULLET_SPEED  :  goto bulletDone

	LDA facing
	AND #32
	BEQ .skipL0174
.condpart44
	LDA bulletX
	CLC
	ADC #BULLET_SPEED
	STA bulletX
 jmp .bulletDone

.skipL0174
.L0175 ;  if bulletSouth then bulletY  =  bulletY  +  BULLET_SPEED  :  goto bulletDone

	BIT facing
	BVC .skipL0175
.condpart45
	LDA bulletY
	CLC
	ADC #BULLET_SPEED
	STA bulletY
 jmp .bulletDone

.skipL0175
.L0176 ;  if bulletWest then bulletX  =  bulletX  -  BULLET_SPEED

	BIT facing
	BPL .skipL0176
.condpart46
	LDA bulletX
	SEC
	SBC #BULLET_SPEED
	STA bulletX
.skipL0176
.bulletDone
 ; bulletDone

.
 ; 

.L0177 ;  player0x  =  heroX

	LDA heroX
	STA player0x
.L0178 ;  player0y  =  heroY

	LDA heroY
	STA player0y
.
 ; 

.L0179 ;  ballx  =  bulletX

	LDA bulletX
	STA ballx
.L0180 ;  bally  =  bulletY

	LDA bulletY
	STA bally
.
 ; 

.L0181 ;  gosub drawMinuteman

 jsr .drawMinuteman

.L0182 ;  gosub drawRedcoat

 jsr .drawRedcoat

.
 ; 

.L0183 ;  COLUBK  =  COLOR_GRASS

	LDA #COLOR_GRASS
	STA COLUBK
.L0184 ;  CTRLPF  =  BALL_WIDTH_2

	LDA #BALL_WIDTH_2
	STA CTRLPF
.L0185 ;  ballheight  =  1

	LDA #1
	STA ballheight
.
 ; 

.L0186 ;  rem if collision(player0, playfield) then scorecolor = COLOR_SCORE_RED

.
 ; 

.L0187 ;  rem mytemp1 = ((heroX-15)/4)

.L0188 ;  rem mytemp2 = ((heroY-1)/8)

.L0189 ;  rem if pfread(mytemp1, mytemp2) then scorecolor = COLOR_SCORE_RED

.
 ; 

.L0190 ;  drawscreen

 jsr drawscreen
.
 ; 

.L0191 ;  goto main

 jmp .main

.
 ; 

.
 ; 

.
 ; 

.drawMinuteman
 ; drawMinuteman

.L0192 ;  player0color:

	LDX #<playercolorL0192_0
	STX player0color
	LDA #>playercolorL0192_0
	STA player0color+1
.
 ; 

.L0193 ;  player0:

	LDX #<playerL0193_0
	STX player0pointerlo
	LDA #>playerL0193_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0194 ;  return

	RTS
.
 ; 

.
 ; 

.
 ; 

.drawRedcoat
 ; drawRedcoat

.L0195 ;  player1color:

	LDX #<playercolorL0195_1
	STX player1color
	LDA #>playercolorL0195_1
	STA player1color+1
.
 ; 

.L0196 ;  player1:

	LDX #<playerL0196_1
	STX player1pointerlo
	LDA #>playerL0196_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0197 ;  return

	RTS
.
 ; 

.
 ; 

.
 ; 

.drawLevel1
 ; drawLevel1

.L0198 ;  pfcolors:

 lda # $02
 sta COLUPF
 ifconst pfres
 lda #>(pfcolorlabel61-132+pfres*pfwidth)
 else
 lda #>(pfcolorlabel61-84)
 endif
 sta pfcolortable+1
 ifconst pfres
 lda #<(pfcolorlabel61-132+pfres*pfwidth)
 else
 lda #<(pfcolorlabel61-84)
 endif
 sta pfcolortable
.L0199 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000001, %00000111
	if (pfwidth>2)
	.byte %00000011, %00000011
 endif
	.byte %00000001, %00000111
	if (pfwidth>2)
	.byte %00000011, %00000011
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000001, %00000111
	if (pfwidth>2)
	.byte %00000011, %00000011
 endif
	.byte %00000001, %00000111
	if (pfwidth>2)
	.byte %00000011, %00000011
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L0200 ;  return

	RTS
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL0192_0
	.byte  $00
	.byte  $00
	.byte  $F0
	.byte  $00
	.byte  $0E
	.byte  $0E
	.byte  $3E
	.byte  $00
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0193_0
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %01011010
	.byte  %01011010
	.byte  %00111100
	.byte  %00011000
	.byte  %00111100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL0195_1
	.byte  $00
	.byte  $00
	.byte  $42
	.byte  $0E
	.byte  $42
	.byte  $42
	.byte  $3E
	.byte  $00
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0196_1
	.byte  %00111100
	.byte  %00011000
	.byte  %00011000
	.byte  %01011010
	.byte  %01011010
	.byte  %00111100
	.byte  %00011000
	.byte  %00111100
 ifconst pfres
 if (<*) > (254-pfres*pfwidth)
	align 256
	endif
 if (<*) < (136-pfres*pfwidth)
	repeat ((136-pfres*pfwidth)-(<*))
	.byte 0
	repend
	endif
 else
 if (<*) > 206
	align 256
	endif
 if (<*) < 88
	repeat (88-(<*))
	.byte 0
	repend
	endif
 endif
pfcolorlabel61
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 .byte  $02,0,0,0
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif

; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
   if bankswitch == 64
     ORG  $10FF0
     RORG $1FFF0
     lda $ffe0 ; we use wasted space to assist stella with EF format auto-detection
     ORG  $10FF8
     RORG $1FFF8
     ifconst superchip 
       .byte "E","F","S","C"
     else
       .byte "E","F","E","F"
     endif
     ORG  $10FFC
     RORG $1FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
