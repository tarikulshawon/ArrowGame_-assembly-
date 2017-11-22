;COLOR TEXT

.MODEL SMALL
.STACK 100h

;#########################
; MACRO

draw_row Macro x,col_s,col_f
    Local l1
    ; draws a line in row x from col 10 to col 630
    MOV AH, 0CH
    MOV AL, 1
    MOV CX, col_s
    MOV DX, x
L1: INT 10h
    INC CX
    CMP CX, col_f
    JL L1
    EndM
    draw_col Macro y,row_s,row_f
    Local l2
    ; draws a line col y from row 10 to row 310
    MOV AH, 0CH
    MOV AL, 1
    MOV CX, y
    MOV DX, row_s
L2: INT 10h
    INC DX
    CMP DX, row_f
    JL L2
    EndM

display_string Macro x,row,column,length

    MOV AX, @DATA
    MOV ES, AX  
      
    MOV AH, 13H ; WRITE THE STRING
    MOV AL, 0H; ATTRIBUTE IN BL, MOVE CURSOR TO THAT POSITION
    XOR BH,BH ; VIDEO PAGE = 0
    MOV BL,  9 ; color
    
    MOV BP, OFFSET x ; ES: BP POINTS TO THE TEXT
    MOV CX, length ; LENGTH OF THE STRING
    MOV DH, row ;ROW TO PLACE STRING
    MOV DL, column ; COLUMN TO PLACE STRING
    INT 10H
    EndM  

;#########################



.DATA 

new_timer_vec   dw  ?,?
old_timer_vec   dw  ?,?
timer_flag  db  0
vel_x       dw  1
vel_y       dw  1
row db 9
set_flag dw 0




Character DB " O",13
          DB "/|\_",13
          DB " |",13
          DB "/ \",0
CharacterX db 0
CharacterY db 10          
          
Balloon DB "0 ",13
        DB "|",13
        DB " |",0
        
balloonX db 40  ; max value of col is 80
balloonY db 30  ; max value of row is 29
color db 181
x db 10
y db 10

msg_missed db "MISSED SHOOT$"
msg_score  db "SCORE$"

.CODE


timer_tick Proc
    PUSH DS
    PUSH AX
    
    MOV AX, Seg timer_flag
    MOV DS, AX
    MOV timer_flag, 1
    
    POP AX
    POP DS
    
    IRET
timer_tick EndP


move_balloon Proc


    CALL draw_balloon

    dec balloonY

    CALL draw_balloon
    
    CALL check_boundary
    
   
    RET 
move_balloon EndP

choice proc near
    
    push ax
    push dx
    
    
    mov ah, 1
    int 16h
    
    jz otherwise
    
    mov ah, 0
    int 16h
    
    cmp ah,48h
    je up
    
    cmp ah,50h
    je down
    
    

otherwise:
   
   mov set_flag,0
   jmp exit
up: 
    
    mov set_flag,1
    jmp exit
down:
    
    mov set_flag,2   

exit:
    
    pop dx
    pop ax
    ret
choice endp


move_character proc near
    call draw_character
    push ax
    
    push cx
    
    mov cx,set_flag
    
    cmp cx,1
    je up1
    
    cmp cx,2
    je down1
    
    cmp cx,0
    je print
    
up1:
    cmp characterY,0
    jne decrement
    jmp print
down1:
   inc characterY
   jmp print
   
decrement:
    dec characterY
    
print:
    mov set_flag, 0
    call draw_character    
    pop cx   
    pop ax
    ;call check_boundary_character
    ret
    
move_character endp


check_boundary Proc
    ;for vga graphics text col 80 and row 29
    LP1:    
        CMP balloonY, 0
        JE LP2
        RET
    LP2:
        CALL draw_balloon; replace balloon
        MOV BALLOONY,30
        RET 
check_boundary EndP

setup_int Proc
; save old vector and set up new vector
; input: al = interrupt number
;    di = address of buffer for old vector
;    si = address of buffer containing new vector
; save old interrupt vector
    push ax
    push bx
    push cx
    push dx

    MOV AH, 35h ; get vector
    INT 21h
    MOV [DI], BX    ; save offset
    MOV [DI+2], ES  ; save segment
; setup new vector
    MOV DX, [SI]    ; dx has offset
    PUSH DS     ; save ds
    MOV DS, [SI+2]  ; ds has the segment number
    MOV AH, 25h ; set vector
    INT 21h
    POP DS
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    RET
setup_int EndP


    
draw_character proc near
  mov  di, offset Character
  mov al, CharacterX
  mov x, al
  mov al, CharacterY
  mov y, al
  
  
  while1:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak ;THEN JUMP TO LINEBREAK.
  cmp  al, 0   ;IF CHAR == 0
  je   finish  ;THEN JUMP TO FINISH.
  
  
  
  mov  ah, 9
  mov  bh, 0
  mov  bl, color  ;ANY COLOR.
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char
linebreak:  
  inc  y  ;MOVE TO NEXT LINE.    
  mov  x, 0  ;X GOES TO THE LEFT.
next_char:
  inc  di  
  jmp  while1    

  
finish :
    ret  

draw_character endp   
 

    
draw_balloon proc near

    push ax
    push bx
    push cx
    push dx
    
    mov  di, offset Balloon
    mov al, balloonX
    mov x, al
    mov al, balloonY
    mov y, al


    while2:      
    ;SET CURSOR POSITION FOR CURRENT CHAR.  
    mov dl, x
    mov dh, y
    mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
    mov bh, 0 ;PAGE.
    int 10h

    mov  al, [ di ]  ;CHAR TO DISPLAY.
    cmp  al, 13    ;IF CHAR == 13
    je   linebreak1 ;THEN JUMP TO LINEBREAK.
    cmp  al, 0   ;IF CHAR == 0
    je   finish1  ;THEN JUMP TO FINISH.


    mov  ah, 9
    mov  bh, 0
    mov  bl, color  ;ANY COLOR.
    mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
    int  10h

    inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
    jmp  next_char1

    linebreak1:  
    inc  y  ;MOVE TO NEXT LINE.
    dec x
    dec x

    next_char1:
    inc  di  
    jmp  while2    
 
    
    finish1:
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    
    draw_balloon endp
    

    
TIMER proc near
        
   tt:
       
       CMP timer_flag, 1
       JNE tt
       MOV timer_flag, 0
       
       CALL move_balloon
       call choice
       CALL move_character
       
 
       mov set_flag,0
       
   
   tt2:
       CMP timer_flag, 1
       JNE tt2
       MOV timer_flag, 0
       JMP tt
     
   ret
   TIMER endp
   


   
    
    


main proc
    MOV AX, @DATA
    MOV DS, AX
    MOV ES, AX
    
    ;set graphics mode
    mov ah,0
    mov al, 12h  ;640x480 16 color 
    int 10h
    
    mov ah ,11
    mov bh ,0
    mov bl,0
    int 10h
    
     ;set up timer interrupt vector
    MOV new_timer_vec, offset timer_tick
    MOV new_timer_vec+2, CS
    MOV AL, 1CH; interrupt type
    LEA DI, old_timer_vec
    LEA SI, new_timer_vec
    CALL setup_int
    
    ;display boarder
    draw_col 450,0,640 
    draw_col 452,0,640
    ;dispaly score
    mov bl,3
    display_string msg_missed,5,63,12
    display_string msg_score,10,63,5
    
    
    ;call the character to draw itself
    
    CALL draw_character
    CALL draw_balloon

    
    
    CALL TIMER
    
    

    
    MOV Ah, 4CH
    INT 21H    

    
main endp
END main
 