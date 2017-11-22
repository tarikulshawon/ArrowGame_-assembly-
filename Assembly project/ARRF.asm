.Model Small
        draw_row Macro x
        LOCAL L1
        LOCAL L2
        ; draws a line in row x from col 10 to col 300
        MOV AH, 0CH
        ;LINE COLOR
        MOV AL, 2
        MOV CX, 10
        MOV DX, x
    L1: 
        INT 10h
        INC CX
        CMP CX, 300
        JL L1
        EndM


        draw_col Macro y
        Local l2
        ; draws a line col y from row 10 to row 189
        MOV AH, 0CH
        ;LINE COLOR
        MOV AL, 2
        MOV CX, y
        MOV DX, 10
        
     L2: 
        INT 10h
        INC DX
        CMP DX, 181
        JL L2
        EndM

.Stack 100h


.Data
        ;FRONT PAGE:
        
        WELCOME_MESSAGE db "Welcome to the SHOOTERS GAME!!",0
        INSTRUCTION_MESSAGE db 0AH,0DH,0AH,0DH,"INSTRUCTIONS:",0AH,0DH,"YOU HAVE TO SHOOT THE TARGET IN THE BOARD",0AH,0DH,"IF YOU MISS THE TARGET 5 TIMES THE GAME WILL BE OVER",0DH,0AH,0AH,0DH,"PRESS ANY KEY TO START_$" 
        
        ;GAME PAGE:
        START_CONDITION_OF_ARROW DB 0
        MISSED_OF_PLAYER_MESSAGE DB "YOU MISSED:  TARGETS$"
        MISSED_OF_PLAYER_LENGTH DB 19;11,12
        SCORE_MESSAGE DB "Score:  AAAA   $"
        SCORE_LENGTH DW ?
        MISSED_OF_PLAYER_SCORE DB -1

        STARTER_OF_ARROW DB 0
        MAIN_SCORE_OF_PLAYER DB 0
        ARROW_ROW DW 60
        RANDOM DW 63
        TEN DB 10
        GAMEOVER DB 0
        HOHO DB 0;1 HOILE BARBE NA
        ;ARROW_COLUM DW 10
        ARROW_COLUM DW 290
        new_timer_vec   dw  ?,?
        old_timer_vec   dw  ?,?
        timer_flag  db  0
        VEL_ARROW DW 10
        vel_x       dw  1
        DIVIDEND_ROW DW 180
        vel_y       dw  5
        TARGET_POSITION DW ?
        FILENAME DB "highscore.txt",0
        HANDLE DB ?
        BUFFER DB 3 DUP(0)
        
        ;END PAGE:
        GAME_OVER_MESSAGE DB "GAME OVER$"
        GAME_OVER_MESSAGE_LENGTH DB 9
.CODE

        set_display_mode Proc
            ; sets display mode and draws boundary
            MOV AH, 0
            MOV AL, 04h; 320x200 4 color
            INT 10h
            ; select palette    
            MOV AH, 0BH
            MOV BH, 1
            MOV BL, 14
            INT 10h
            ; set bgd color
            MOV BH, 0
            ;BACKGROUND COLOR , blue = 1, green = 2, red = 4, black = 0
            MOV BL,2
            INT 10h
            ; draw boundary
            draw_row 09
            draw_row 181
            draw_col 10
            draw_col 300
            
             draw_row 11
             draw_row 179
             draw_col 12
             draw_col 298


            RET
        set_display_mode EndP

        DISPLAY_ARROW Proc NEAR
            ; displays ARROW at col CX and row DX with color given in AL
            ; input: AL = color of ARROW
            ;    CX = col
            ;    DX = row
            PUSH CX
            PUSH DX
            MOV CX,ARROW_COLUM
            MOV DX,ARROW_ROW
            MOV AH, 0CH ; write pixel
            INT 10h
            ;INC CX      ; pixel on next col
            ;INT 10h
            ;INC DX      ; down 1 row
            ;INT 10h
            ;DEC CX      ; prev col
            ;INT 10h
            PUSH BX
            MOV BX,20
            CHOL:
                INC CX
                INT 10H
                DEC BX
                JNE CHOL

            MOV DX,ARROW_ROW
            MOV CX,ARROW_COLUM
            ADD CX,20
            MOV BX,5
            DRAWING_UPPER_ARROW:
                INT 10H
                DEC CX
                DEC DX
                DEC BX
                JNE DRAWING_UPPER_ARROW
            MOV DX,ARROW_ROW
            MOV CX,ARROW_COLUM
            ADD CX,20
            MOV BX,5
            DRAWING_LOWER_ARROW:
                INT 10H
                DEC CX
                INC DX
                DEC BX
                JNE DRAWING_LOWER_ARROW
            POP BX
            POP DX
            POP CX



            ; restore dx
            RET 
            DISPLAY_ARROW ENDP
        
        DISPLAY_BALL Proc
            ; displays ball at col CX and row DX with color given in AL
            ; input: AL = color of ball
            ;    CX = col
            ;    DX = row
            MOV AH, 0CH ; write pixel
            INT 10h
            ;  INC CX      ; pixel on next col
            ;INT 10h
            ;INC DX      ; down 1 row
            ;INT 10h
            ;DEC CX      ; prev col
            ;INT 10h
            PUSH BX
            MOV BX,30

            CHOLO:
                INC DX
                INT 10H
                DEC BX
                JNE CHOLO
            SUB DX,30 
            DEC CX
            ADD DX,10
            MOV BX,10
            CHOL_2:
                INC DX
                INT 10H
                DEC BX
                JNE CHOL_2
            SUB DX,20
            ADD CX,1
            POP BX
            ; restore dx
            RET 
        DISPLAY_BALL EndP

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

        MOVE_BALL Proc
            ; erase ball at current position and display at new position
            ; input: CX = col of ball position
            ;    DX = rwo of ball position
            ; erase ball
            MOV AL, 0
            CALL display_ball
            CALL DISPLAY_ARROW
            ; get new position
            ;  ADD CX, vel_x
            ADD DX, vel_y
            CMP START_CONDITION_OF_ARROW,1
            JE NEXT_
            PUSH AX
            MOV AX,VEL_ARROW
            ADD ARROW_COLUM,AX
            POP AX
            ; check boundary

            CALL CHECK_BOUNDARY_OR_TARGET_FOR_ARROW
            NEXT_:
                CALL check_boundary
                ; wait for 1 timer tick to display ball
            test_timer:
                CMP timer_flag, 1
                JNE test_timer

                MOV timer_flag, 0
                MOV AL, 3
                CALL display_ball
                CALL DISPLAY_ARROW
            RET 
        MOVE_BALL EndP
        
        
        CHECK_BOUNDARY_OR_TARGET_FOR_ARROW PROC NEAR
            PUSH AX
            PUSH CX
            PUSH DX
            ADD ARROW_COLUM,10
            ;;;DEC CX
            SUB CX,5
            CMP ARROW_COLUM,CX
            JL CHECKING_BOUNDARY_
            ;; JNE CHECKING_BOUNDARY
            ;;;ADD DX,12
            CMP ARROW_ROW,DX
            JL CHECKING_BOUNDARY_
            ;;;ADD DX,4
            ADD DX,30
            CMP ARROW_ROW,DX
            JG CHECKING_BOUNDARY_
            INC MAIN_SCORE_OF_PLAYER
            MOV HOHO,1
            JMP CHECKING_BOUNDARY
            CHECKING_BOUNDARY_:

            ;SCORE UPDATE
            CHECKING_BOUNDARY:

            LP2_:    

                CMP ARROW_COLUM,280
                JG LP3_
                SUB ARROW_COLUM,10
                JMP DONE_
            LP3_:   
                MOV ARROW_COLUM,10
                ; PUSH AX

                PUSH AX
                PUSH DX
            CHANGING_ROW:
                MOV AX,RANDOM
                MOV DX,0
                ADD RANDOM,AX
                MOV AX,RANDOM
                DIV DIVIDEND_ROW
                MOV RANDOM,DX
                MOV AX,10
                CMP RANDOM,AX
                JL CHANGING_ROW
            MOV AX,RANDOM
            MOV ARROW_ROW,AX
            POP DX
            POP AX
            CMP HOHO,1;;1 HOILE BARBE NA
            JE RRR
            INC MISSED_OF_PLAYER_SCORE
            PUSH BX
            MOV BL,5
            CMP MISSED_OF_PLAYER_SCORE,BL
            JL RRR2

            MOV gameover,1
            MOV HOHO,0
            POP BX
            ;DEC MAIN_SCORE_OF_PLAYER
            MOV START_CONDITION_OF_ARROW,1
            JMP DONE_
            RRR2:
                POP BX
            RRR:
                MOV HOHO,0
                ;POP BX
                ;DEC MAIN_SCORE_OF_PLAYER
                MOV START_CONDITION_OF_ARROW,1
                JMP DONE_


            DONE_:

                POP DX
                POP CX
                POP AX

            RET 
            CHECK_BOUNDARY_OR_TARGET_FOR_ARROW ENDP
            
        CHECK_BOUNDARY Proc
            ; determine if ball is outside screen, if so move it back in and 
            ; change ball direction
            ; input: CX = col of ball
            ;    DX = row of ball
            ; output: CX = valid col of ball
            ;     DX = valid row of ball
            ; check col value
            ;CMP CX, 11
            ;JG LP1
            ;MOV CX, 11
            ;NEG vel_x
            ;JMP LP2 
            ;LP1:    CMP CX, 298
            ;JL LP2
            ;MOV CX, 298
            ;NEG vel_x
            ; check row value
            LP2:    
                CMP DX, 14
                JG LP3
                MOV DX, 14
                NEG vel_y
                JMP done
            LP3:    
                CMP DX, 150
                JL done
                MOV DX, 150
                NEG vel_y
            done:
                RET 
        CHECK_BOUNDARY EndP

        setup_int Proc
            ; save old vector and set up new vector
            ; input: al = interrupt number
            ;    di = address of buffer for old vector
            ;    si = address of buffer containing new vector
            ; save old interrupt vector
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
            RET
        setup_int EndP

        MAIN Proc
            mov ax, @data
            mov ds, ax 

            mov ax, 0b800H
            mov es, ax

            ;clearing the screen
            mov ax, 0003H
            int 10H
            lea BX, WELCOME_MESSAGE
            mov dx,00
            call writestringat

            lea dx, INSTRUCTION_MESSAGE
            mov ah, 09H
            int 21h

            mov ah, 07h
            int 21h
            mov ax, 0003H
            int 10H


            MOV AX, @data
            MOV DS, AX

            ; set graphics display mode & draw border
            CALL set_display_mode
            ; set up timer interrupt vector
            MOV new_timer_vec, offset timer_tick
            MOV new_timer_vec+2, CS
            MOV AL, 1CH; interrupt type
            LEA DI, old_timer_vec
            LEA SI, new_timer_vec
            CALL setup_int
            ; start ball at col = 298, row = 100
            ; for the rest of the program CX = ball row, DX = ball col
            MOV CX, 290
            MOV DX, 10
            MOV AL, 3
            CALL display_ball
            CALL DISPLAY_ARROW
            ; wait for timer tick before moving the ball
            tt:
                CMP timer_flag, 1
                JNE tt
                MOV timer_flag, 0
                PUSH AX
                MOV AL,1
                CMP GAMEOVER,AL
                JE QUIT
                POP AX
                CALL move_ball
                CALL PRINT_SCORE
                CALL PRINT_MISSED_OF_PLAYER_SCORE


                CMP START_CONDITION_OF_ARROW,1
                JNE tt2
                PUSH DX
                PUSH AX
                CALL CHECK_FOR_KEYBOARD
                POP AX
                POP DX

            tt2:
                CMP timer_flag, 1
                JNE tt2
                MOV timer_flag, 0
                JMP tt

                ;POP AX 
            QUIT:
                POP AX
                ; displays ball at col CX and row DX with color given in AL
                ; input: AL = color of ball
                ;    CX = col
                ;    DX = row

                MOV AL,5

                MOV AH, 0CH ; write pixel
                INT 10h
                ;  INC CX      ; pixel on next col
                ;INT 10h
                ;INC DX      ; down 1 row
                ;INT 10h
                ;DEC CX      ; prev col
                ;INT 10h
                MOV CX,0
                MOV DX,0
                MOV BX,65000
            KOOP:
                INT 10H
                INC CX
                CMP CX,BX
                JNE KOOP
                MOV AX,@DATA
                MOV DS,AX
                MOV AH ,3DH
                MOV AL,0
                LEA DX, FILENAME
                INT 21H

                JC ERROR
                CALL WRITE_GAMEOVER_MESSAGE
                CALL PRINT_SCORE



                MOV AH,0
                INT 16H

            ERROR:
                MOV AH,4CH
                INT 21H
        MAIN EndP

        ;dl contains the ascii character if keypressed, else dl contains 0
        ;uses dx and ax, preserves other registers
        READCHAR proc
            mov ah, 01H
            int 16H
            jnz KEYBDPRESSED
            xor dl, dl
            ret
            KEYBDPRESSED:
                ;extract the keystroke from the buffer
                mov ah, 00H
                int 16H
                mov dl,al
                ret


        READCHAR endp  
     
        
        CHECK_FOR_KEYBOARD PROC NEAR
            CALL READCHAR
            CMP DL, 0
            JE NOT_PRESSED_ANY_KEY

            ;so a key was pressed, which key was pressed then solti?
            cmp DL,'A'
            JNE NOT_PRESSED_ANY_KEY
            ;MOV STARTER_OF_ARROW,1
            MOV START_CONDITION_OF_ARROW,0
            RET
            NOT_PRESSED_ANY_KEY:
            RET
        CHECK_FOR_KEYBOARD ENDP


        ;dx contains row, col
        ;bx contains the offset of the string
        writestringat proc
            push dx
            mov ax, dx
            and ax, 0FF00H
            shr ax,8
            push bx
            mov bh, 160
            mul bh

            pop bx
            and dx, 0FFH
            shl dx,1
            add ax, dx
            mov di, ax
            loop_writestringat:

                mov al, [bx]
                test al, al
                jz exit_writestringat
                mov es:[di], al
                inc di
                inc di
                inc bx
                jmp loop_writestringat


            exit_writestringat:
                pop dx
                ret


        writestringat endp
        
        
        
        PRINT_SCORE PROC NEAR
        
            PUSH AX
            PUSH BX
            PUSH DX
            PUSH CX
            MOV DL,20
            LEA SI,SCORE_MESSAGE
            DEC SI
            CLD
            MOV SCORE_LENGTH,8

            CMP MAIN_SCORE_OF_PLAYER,0
            ;JL NEGATIVE_SIGN
            PUSH AX
            MOV AH,0
            MOV AL,MAIN_SCORE_OF_PLAYER
            DIV TEN
            ADD AL,30H
            MOV [SI+7],AL
            ADD AH,30H
            MOV [SI+8],AH
            POP AX
            ;;MOV BL,MAIN_SCORE_OF_PLAYER
            ;;ADD BL,30H
            ;;MOV [SI+7],BL
            ;MOV [SI+7],'+'
            JMP SCORE_PRINT_LOOP

            ;NEG MAIN_SCORE_OF_PLAYER
            SCORE_PRINT_LOOP:
                INC SI
                MOV AH,02 ;set cursor
                MOV BH,0 ;page 0
                MOV DH,0 ;row 0

                INT 10H
                MOV AH,9 ;write char function


                ;INC SI
                MOV  AL,[SI]
                MOV  BL,2

                MOV CX,1
                INT 10H
                INC DL
                DEC SCORE_LENGTH
                JNE SCORE_PRINT_LOOP
            POP CX
            POP DX
            POP BX
            POP AX
            RET
        PRINT_SCORE ENDP
       
        
        PRINT_MISSED_OF_PLAYER_SCORE PROC NEAR



            PUSH AX
            PUSH BX
            PUSH DX
            PUSH CX
            MOV DL,0
            LEA SI,MISSED_OF_PLAYER_MESSAGE
            DEC SI
            MOV MISSED_OF_PLAYER_LENGTH,19
            CLD
            PUSH AX
            MOV AH,0
            MOV AL,MISSED_OF_PLAYER_SCORE
            CMP AL,-1
            JNE RR
            MOV AL,0
            RR:
                DIV TEN
                ADD AL,30H
                MOV [SI+12],AL
                ADD AH,30H
                MOV [SI+13],AH
                POP AX
            SCORE_PRINT_LOOP_:
                INC SI
                MOV AH,02 ;set cursor
                MOV BH,0 ;page 0
                MOV DH,0 ;row 0

                INT 10H
                MOV AH,9 ;write char function


                ;INC SI
                MOV  AL,[SI]
                MOV  BL,2

                MOV CX,1
                INT 10H
                INC DL
                DEC MISSED_OF_PLAYER_LENGTH
                JNE SCORE_PRINT_LOOP_
            POP CX
            POP DX
            POP BX
            POP AX
            RET
        PRINT_MISSED_OF_PLAYER_SCORE ENDP 
        
        
        WRITE_GAMEOVER_MESSAGE PROC NEAR



            PUSH AX
            PUSH BX
            PUSH DX
            PUSH CX
            MOV DL,0
            LEA SI,GAME_OVER_MESSAGE
            DEC SI
            MOV GAME_OVER_MESSAGE_LENGTH,5
            CLD
            ;PUSH AX
            ;MOV AH,0
            ;MOV AL,MISSED_OF_PLAYER_SCORE
            ;CMP AL,-1
            ;JNE RR
            ;MOV AL,0
            ;RR:
            ;DIV TEN
            ;ADD AL,30H
            ;MOV [SI+12],AL
            ;ADD AH,30H
            ;MOV [SI+13],AH
            ;POP AX
            SCORE__PRINT_LOOP_:
                INC SI
                MOV AH,02 ;set cursor
                MOV BH,0 ;page 0
                MOV DH,0 ;row 0

                INT 10H
                MOV AH,9 ;write char function


                ;INC SI
                MOV  AL,[SI]
                MOV  BL,2

                MOV CX,1
                INT 10H
                INC DL
                DEC GAME_OVER_MESSAGE_LENGTH
                JNE SCORE__PRINT_LOOP_
            POP CX
            POP DX
            POP BX
            POP AX
            RET
        WRITE_GAMEOVER_MESSAGE ENDP 
        END MAIN
