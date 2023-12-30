.segment "HEADER"
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00000000 ; mapper and mirroring
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00 ; filler bytes
.segment "ZEROPAGE" ; LSB 0 - FF
buttons1: .res 1
buttons2: .res 1
spritePointer: .res 1

SRITEPAGE = $200

.segment "STARTUP"


;-----------------------------------------------------------------------------------------------------------------------------

Reset:
    SEI ; Disables all interrupts
    CLD ; disable decimal mode

    ; Disable sound IRQ
    LDX #$40
    STX $4017

    ; Initialize the stack register
    LDX #$FF
    TXS ; Transfer X to the stack pointer (FF is the top of the stack)

    INX ; #$FF + 1 => #$00

    ; Zero out the PPU registers
    STX $2000
    STX $2001

    STX $4010

;-----------------------------------------------------------------------------------------------------------------------------

:
    BIT $2002
    BPL :-

    TXA

;-----------------------------------------------------------------------------------------------------------------------------

CLEARMEM:
    STA $0000, X ; $0000 => $00FF
    STA $0100, X ; $0100 => $01FF
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    LDA #$FF
    STA $0200, X ; $0200 => $02FF
    LDA #$00
    INX
    BNE CLEARMEM    
; wait for vblank
:
    BIT $2002
    BPL :-

    LDA #$02
    STA $4014
    NOP

    ; $3F00
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00

;-----------------------------------------------------------------------------------------------------------------------------

LoadPalettes:
    LDA PaletteData, X
    STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
    INX
    CPX #$20
    BNE LoadPalettes

    LDX #$00

;-----------------------------------------------------------------------------------------------------------------------------

LoadSprites:
    LDA SpriteData+1
    STA spritePointer

    LDA SpriteData, X
    STA $0200, X
    INX
    CPX #$20
    BNE LoadSprites 

; Enable interrupts
    CLI

    LDA #%10010000 ; enable NMI change background to use second chr set of tiles ($1000)
    STA $2000
    ; Enabling sprites and background for left-most 8 pixels
    ; Enable sprites and background
    LDA #%00011110
    STA $2001

;-----------------------------------------------------------------------------------------------------------------------------

Loop:
    JMP Loop

;-----------------------------------------------------------------------------------------------------------------------------

NMI:
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display
    STA $4014

    JSR ReadController1  ; get the current button data for player 1
    JSR ReadController2  ; get the current button data for player 2
    JSR MoveSprite

    RTI


;-----------------------------------------------------------------------------------------------------------------------------

ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadController2Loop
  RTS  

;-----------------------------------------------------------------------------------------------------------------------------

MoveSprite:
    LDX buttons1
    CPX #%01000000 ; Check if the A button is pressed 
    BEQ MoveSpriteRight
    BNE SetFirstSpritePointer
    RTS

; Sprite X location is stored at $0203 (and then every 4th address after)
; So we create a loop, updating the X location for all 20 sprites that make up Mario
MoveSpriteRight:
    JSR SetSecondSpritePointer
    CLC
    LDX #$0
    :
        LDA $0203, X
        ADC #$1
        STA $0203, X
        
        INX
        INX
        INX
        INX
        CPX #$80
        BNE :-
        RTS


LoadFirstSprite:
    LDX #$0

    :
    LDA SpriteData, X
    STA $0200, X
    INX
    CPX #$20
    BNE :-
    RTS 

LoadSecondSprite:
    LDX #$0

    :
    LDA SpriteData+32, X
    STA $0200, X
    INX
    CPX #$20
    BNE :-
    RTS

SetFirstSpritePointer:
    LDA #$1
    STA spritePointer
    JSR LoadSpriteData
    RTS

SetSecondSpritePointer:
    LDA #$33
    STA spritePointer
    JSR LoadSpriteData
    RTS

LoadSpriteData:
    LDX spritePointer

    :
    LDA SpriteData, X
    STA $0201
    LDA SpriteData, X
    STA $0205
    LDA SpriteData, X
    STA $0209
    LDA SpriteData, X
    STA $0213
    LDA SpriteData, X
    STA $0217
    LDA SpriteData, X
    STA $0221
    LDA SpriteData, X
    STA $0225
    LDA SpriteData, X
    STA $0229

    INX
    INX
    INX
    INX

    CPX spritePointer
    BNE :-

    RTS

;-----------------------------------------------------------------------------------------------------------------------------

PaletteData:
  .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F  ;background palette data
  .byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data

SpriteData:
    ; Foot forward keyframe
    .byte $08, $00, $00, $08 ;201
    .byte $08, $01, $00, $10 ;205
    .byte $10, $02, $00, $08 ;209
    .byte $10, $03, $00, $10 ;213
    .byte $18, $04, $00, $08 ;217
    .byte $18, $05, $00, $10 ;221
    .byte $20, $06, $00, $08 ;225
    .byte $20, $07, $00, $10 ;229

    ; Foot down keyframe
    .byte $08, $08, $00, $08
    .byte $08, $09, $00, $10
    .byte $10, $0A, $00, $08
    .byte $10, $0B, $00, $10
    .byte $18, $0C, $00, $08
    .byte $18, $0D, $00, $10
    .byte $20, $0E, $00, $08
    .byte $20, $0F, $00, $10

    ; Mid run keyframe
    .byte $08, $10, $00, $08
    .byte $08, $11, $00, $10
    .byte $10, $12, $00, $08
    .byte $10, $13, $00, $10
    .byte $18, $14, $00, $08
    .byte $18, $15, $00, $10
    .byte $20, $16, $00, $08
    .byte $20, $17, $00, $10

    ; Scared keyframe
    .byte $08, $18, $00, $08
    .byte $08, $19, $00, $10
    .byte $10, $1A, $00, $08
    .byte $10, $1B, $00, $10
    .byte $18, $1C, $00, $08
    .byte $18, $1D, $00, $10
    .byte $20, $1E, $00, $08
    .byte $20, $1F, $00, $10

    ; Jump keyframe
    .byte $08, $20, $00, $08
    .byte $08, $21, $00, $10
    .byte $10, $22, $00, $08
    .byte $10, $23, $00, $10
    .byte $18, $24, $00, $08
    .byte $18, $25, $00, $10
    .byte $20, $26, $00, $08
    .byte $20, $27, $00, $10

.segment "VECTORS"
    .word NMI
    .word Reset

.segment "CHARS"
    .incbin "hellomario.chr"

;-----------------------------------------------------------------------------------------------------------------------------