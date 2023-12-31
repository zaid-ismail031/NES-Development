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

SPRITEX = 8
SPRITEY = 80

MUSHROOMX = 80
MUSHROOMY = 80

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
    LDA #$1
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

    LDY #$0

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
    JSR LoadSpriteData
    ;JSR InitializeMushroom
    
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
    RTS

; Sprite X location is stored at $0203 (and then every 4th address after)
; So we create a loop, updating the X location for all 20 sprites that make up Mario
MoveSpriteRight:
    LDA #$0
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
        JSR ANIMATION
        RTS

;-----------------------------------------------------------------------------------------------------------------------------

SetFirstSpritePointer:
    LDA #$1
    STA spritePointer
    RTS

SetSecondSpritePointer:
    LDA #$21
    STA spritePointer
    CPY #$10
    BEQ ResetY
    RTS

LoadSpriteData:
    LDX spritePointer

    LDA SpriteData, X
    STA $0201
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $0205
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $0209
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $020D
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $0211
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $0215
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $0219
    INX
    INX
    INX
    INX

    LDA SpriteData, X
    STA $021D
    INX
    INX
    INX
    INX

    RTS

;InitializeMushroom:
;    LDX #$40
;    LDA SpriteData, X
;    STA $0221, X
;    INX
;    CPX #$60
;    BNE InitializeMushroom
;    RTS
;
;
;LoadMushroom:
;    LDX #$41
;
;    LDA SpriteData, X
;    STA $0221
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $0225
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $0229
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $022D
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $0231
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $0235
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $0239
;    INX
;    INX
;    INX
;    INX
;
;    LDA SpriteData, X
;    STA $024D
;    INX
;    INX
;    INX
;    INX
;
;    RTS


;-----------------------------------------------------------------------------------------------------------------------------

ANIMATION:
    INY
    CPY #$5
    BCC SetFirstSpritePointer
    BEQ SetSecondSpritePointer
    JSR SetSecondSpritePointer
    RTS

ResetY:
    LDY #$0
    RTS

PaletteData:
  .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F  ;background palette data
  .byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data

SpriteData:
    ; Foot forward keyframe
    .byte SPRITEY, $00, $00, SPRITEX
    .byte SPRITEY, $01, $00, SPRITEX+8
    .byte SPRITEY+8, $02, $00, SPRITEX
    .byte SPRITEY+8, $03, $00, SPRITEX+8
    .byte SPRITEY+16, $04, $00, SPRITEX
    .byte SPRITEY+16, $05, $00, SPRITEX+8
    .byte SPRITEY+24, $06, $00, SPRITEX
    .byte SPRITEY+24, $07, $00, SPRITEX+8

    ; Foot down keyframe
    .byte SPRITEY, $08, $00, SPRITEX
    .byte SPRITEY, $09, $00, SPRITEX+8
    .byte SPRITEY+8, $0A, $00, SPRITEX
    .byte SPRITEY+8, $0B, $00, SPRITEX+8
    .byte SPRITEY+16, $0C, $00, SPRITEX
    .byte SPRITEY+16, $0D, $00, SPRITEX+8
    .byte SPRITEY+24, $0E, $00, SPRITEX
    .byte SPRITEY+24, $0F, $00, SPRITEX+8

    ; Mushroom
    .byte MUSHROOMY, $71, $00, MUSHROOMX
    .byte MUSHROOMY, $72, $00, MUSHROOMX+8
    .byte MUSHROOMY+8, $73, $00, MUSHROOMX
    .byte MUSHROOMY+8, $74, $00, MUSHROOMX+8
    .byte MUSHROOMY+16, $75, $00, MUSHROOMX
    .byte MUSHROOMY+16, $76, $00, MUSHROOMX+8
    .byte MUSHROOMY+24, $77, $00, MUSHROOMX
    .byte MUSHROOMY+24, $78, $00, MUSHROOMX+8

.segment "VECTORS"
    .word NMI
    .word Reset

.segment "CHARS"
    .incbin "hellomario.chr"

;-----------------------------------------------------------------------------------------------------------------------------