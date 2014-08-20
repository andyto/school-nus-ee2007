; EE2007 Project 4
; Name: Andy To On Kin
;=======================================================================================================================
; Data Segment
;=======================================================================================================================
DATA SEGMENT
;For video
	current_videomode db	?
;RS-232 Variables 
	rbr	equ	3f8h	    ; Receive Buffer Register
	thr equ	3f8h	    ; Transmitter Holding Register
	dll equ 3f8h	    ; Divison Latch Register (Lower Byte)
	dlm equ 3f9h	    ; Divison Latch Register (Higher Byte)
	ier equ 3f9h	    ; Interrupt Enable Register
	iir equ 3fah	    ; Interrupt Identification Register
	lcr equ 3fbh	    ; Line Control Register
	mcr equ 3fch	    ; Modem Control Register
	lsr equ 3fdh	    ; Line Status Register
	msr equ 3feh	    ; Modem Status Register
	scr equ 3ffh  		; Scratch Pad Register
	comm_port_set equ 0	;0 for com 1; 1 for com 2
	to_be_send	db	?	;temp storage for byte to be send through serial
;For delay
	timedvalue 		dw 	0	 	; timedvalue contains the number of iterations the delay loop must repeat in order to waste 1/18.2 seconds.
	RTC2			dw	0	 	; RTC2 is a dummy var used by the Delay routine to simulate accessing a BIOS var.
	PPI_B			equ	061h	; B-port of PPI
	RTC				equ	es:[6ch]; BIOS Timer Variable (for Real time clock) at 40h:6Ch
	PIT_CW			equ	043h	; PIT control word
	PIT_Ch2			equ	042h	; PIT counter #2	
; Message Variables
	msg_hand_start		db	'Waiting for Handshaking.'
	msg_hand_done		db	'Handshaking complete.'
	msg_liketo		db	'Would you like to play at level:'
	msg_lives 		db 	'Lives Left:'
	msg_gameover 	db 	'Game over!'
	msg_score 		db 	'Score:'
	msg_hiscore 	db	'High Score:'
	msg_single		db	'Single Player Mode'
	msg_multi		db	'Mulitplayer Mode'
	msg_lvl			db	'Level:'
	msg_welcome		db	'Welcome to type till you sweat!'
	msg_press_key	db	'Press any key to continue.'
	msg_menu_pls	db	'Please select:'
	msg_menu_1		db	'1 => Single Player Game'
	msg_menu_2		db	'2 => Mutli-Player Game'
	msg_menu_3		db	'3 => Exit'
	msg_diff		db	'Please enter difficulty level:'
	msg_09			db	'(0-9)'
	msg_paused		db	'Game Paused! Press e to exit.'
	msg_paused2		db	'Game Paused!'
	msg_y			db	'Press y to accept.'
	msg_win			db	'You are the winner!'
;Drawing attributes, use for pass by memory for drawing functions
	_x			db 	0
	_y 			db 	0
	_colour 	db 	0
	_textlt 	dw 	11
	_ht 		db 	0
	_lt 		db 	0
	_text 		dw 	0
	_char 		db 	0
;Game Variables
	dice_tx		db	0		;use for random no. generation
	game_state	db	?		;store the state of the game
	agree_state db	0		;flag to show which side is the one requesting the game
	game_lvl	db	0		;store current game lvl
	game_speed	db	0		;store current game speed
	game_inc	db	0		;store current drop rate
	game_spd_set	db	6,5,4,5,4,4,3,3,2,2,2,2,1,1		;Edit to change speed for single player game
	game_inc_set	db	4,4,5,3,4,3,4,3,4,3,2,1,2,1		;Edit to change drop rate for single player game
	mgame_spd_set	db	9,8,7,6,5,4,4,3,3,2,2,2,1,1		;Edit to change speed for multiplayer game	
	mgame_inc_set	db	10,8,8,8,7,7,7,6,6,5,4,3,2,1	;Edit to change drop rate for multiplayer game	
	play_char	db	16 dup	(?)		;store the character for each column
	play_loc	db	16 dup	(0)		;store how far they have drop/location
	play_drop	db	16 dup	(0)		;store information regarding if they are dropping
	play_add	db	0		;counter for when to drop
	play_life	db	0		;store current life left
	play_score	dw	0		;store current score
	play_hi_score	dw	0		;store the high score
	play_drop_count	db	0		;store the no. of dropping char
	play_kills		db	0		;store the no. ok kills in that lvl
	play_quit		db	0		;flag to show that quit is initated
	play_agree		db	0		;flag to show that user agree with the lvl
	play_pause		db	0		;flag to show that the game is paused
	play_win		db	0		;flag to show that the game is won
	cheat_slow		db	'slow$'		;store the cheat 'slow'
	cheat_slow_count db	0			;ptr to count the progress of the cheat
	cheat_change		db	'change$'	;store the cheat 'change'
	cheat_change_count db	0			;ptr to count the progress of the cheat
	
DATA ENDS
;=======================================================================================================================
; Stack Segment
;=======================================================================================================================
STK SEGMENT STACK
DB 1024 DUP (?) ; 1 Kb Stack. Do not make this stack too small!
TOS LABEL WORD ; A label to indicate the Top-Of-Stack for initialization
STK ENDS

;=======================================================================================================================
; Code Segment
;=======================================================================================================================
CODE SEGMENT
ASSUME CS:CODE, DS:DATA, SS:STK

start: ; Entry Point
; DS, SS and SP initializatoins
MOV DX, DATA
MOV DS, DX
MOV DX, STK
MOV SS, DX
MOV SP, OFFSET TOS

;storing old interrupts------
		oldint0c		dd	?	;For storing old vector address
		oldint9			dd	?	;For storing old vector address

;%%%%%%%%%%%%%%%%%%%%%  Main Code  %%%%%%%%%%%%%%%%%%%%% 
; mainly consist of game main menu, used to link
;different section of the program together
;%%%%%%%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%% 
; Get current video mode, for restoration later 
	mov ah, 0fh
	mov current_videomode, al 

; change video mode 80 x 25, 16 color text
	mov ax, 0003h
	int 10h
	
;start of game
	call cs:game_11_welcome	;call game welcome screen
	call cs:INITCYCLE		;call to find timedvalue for delay function
main_menu:
	call cs:game_12_menu	;call main menu
		cmp al, 31h			;if 1 is pressed 
			je main_sgame	;jump to single player section
		cmp al, 32h			;if 2 is pressed
			je main_mgame	;jump to multiplayer section
		cmp al, 33h			;if 3 is pressed
			je exit			;initate exit

main_sgame:		;single player section
	call cs:game_21_sgame_start		;call single player game
	jmp main_menu		;jmp back to main menu when game ends
	
main_mgame:		;multiplayer section
	call cs:game_31_mgame_start		;call mulitplayer game
	jmp main_menu		;jmp back to main menu when game ends

exit:			;Exit point
; Return original video mode
	mov ah,00
	mov al, current_videomode
	int 10h
	
; Interrupt function number for exit	
	MOV AH, 4Ch 
	INT 21h

;%%%%%%%%%%%%%%%%%%%  Game procedures  %%%%%%%%%%%%%%%%%%% 
; 4 main parts	- common game procedures
;			- mgame (multiplayer)
;			- sgame (single player)
;			- main menu
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

;////////////////////   Common game procedures   ////////////////////

; ----------game_set_lvl ----------
; set and modify game level speed and increase speed
;input - game_lvl(current game level)
game_set_lvl	proc	far
	xor bx,bx			;clear bx for use as index
	mov	bl,game_lvl		;copy current game lvl to bl as lvl info. is stored in array arranged acccording to lvl
	
	mov al,game_inc_set[bx]		;read new drop rate to al
	mov game_inc,al				;mov new drop rate to game_inc

	mov al,game_spd_set[bx]		;read new speed to al
	mov game_speed,al			;mov new speed to game_speed
	
	ret
game_set_lvl	endp

; ----------game_crash ----------
; detect character that have hits the bottom of the screen, update accordingly
;input - play_loc(how far each char have dropped)
game_crash	proc	far
	xor cx,cx			;clear cx for use as counter
	xor bx,bx			;clear bx for use as index
	mov cx,16			; mov 16 into cx, required to loop 16 times
	sgame_end_loop:		;start of loop
		cmp play_loc[bx],22			;compare play_loc, the char location with 22 the last line of play area
			jne game_crash_no		;skip to the next loop if it has not hit the bottom
				dec play_life		;do if char hit bottom =>	 dec play_life(current life left)
				dec play_drop_count			;dec play_drop_count (character is del from dropping)
				sub	play_score,15			;sub play_score (current score) by 15 
				mov	play_loc[bx],0			;regnerate char =>	 change play_loc to 0 (back at the top)
				mov play_drop[bx],0						;change play_drop to 0 (not dropping)
				call cs:rnd_char			;call procedure to generate the missing char
				call cs:display_right		;update score, as it is changed
		game_crash_no:
		inc bx			; inc index
	loop sgame_end_loop

	ret
game_crash	endp

; ----------game_drop ----------
; move character that is dropping by 1 unit down
;input - play_drop( check to see if the are dropping)
game_drop	proc	far
	xor bx,bx			;clear bx for use as index
	xor cx,cx			;clear cx for use as counter
	mov cx,16			; mov 16 into cx, required to loop 16 times
	game_drop_loop:
		cmp play_drop[bx],0		;check if character is dropping by checking play_drop = 1(if dropping)
			je game_drop_no		;skip if it is not dropping
				inc play_loc[bx]	;inc play_loc by 1 if it is dropping
		game_drop_no:
		inc bx		;inc index
	loop game_drop_loop

	ret
game_drop	endp

; ----------game_add ----------
; randomly pick a character and change it to dropping status
;modify play_loc
game_add	proc	far
	sgame_rnd:
		cmp play_drop_count,10		;check to see if there is 10 char dropping
			 je game_add_full		;if yes do not add new ones
		
				mov dl,16			;generate a random no. from 0-16
				call cs:rnd_num		;call the random function
		
				xor bx,bx			;check to see if generated no. is already occupied
				mov bl,al			;by a drop if yes, regenerate until a empty one is obtained
				cmp	play_drop[bx],1		;compare to see if it is occupy
				je sgame_rnd
				
					mov play_drop[bx],1		;if not occupied change the status to dropping
					inc play_drop_count		;increase the drop count

	game_add_full:
	ret
game_add	endp

;////////////////////   Multi player game procedures   ////////////////////

; ----------mgame_set_lvl ----------
; set and modify game level speed and increase speed, same as above except that if uses another array 
;as mgame lvl info is different from sgame info.
;input - game_lvl(current game level)
mgame_set_lvl	proc	far
		xor bx,bx			;clear bx for use as index
		mov	bl,game_lvl		;copy current game lvl to bl as lvl info. is stored in array arranged acccording to lvl
		
		mov al,mgame_inc_set[bx]		;read new drop rate to al
		mov game_inc,al				;mov new drop rate to game_inc
		
		mov al,mgame_spd_set[bx]		;read new speed to al
		mov game_speed,al			;mov new speed to game_speed
		
		ret
mgame_set_lvl	endp

; ----------game_32_mgame ----------
; actual mulitplayer game
;game state: 32h
game_32_mgame proc far
	mov game_state,32h	;set game_state to 32h
	
	game_32_mgame_loop:		;main loop of the game, infinity looped until conditions are met
	
		cmp play_add,0				;section used to determine when to add a new charater
			jne mgame_add_no		;checks play_add and only add if it is 0
			call cs:game_add		;play_add will be inc 1 when it is in the loop
		mgame_add_no:				;play_add will be reset to 0 if it is equal to game_add
			inc play_add			;when play_add = game_add, meaing no. of "game_add" 
			mov al,play_add			;loop has passed
			cmp al,game_inc
				jl mgame_add_no_reset
					mov play_add,0
			mgame_add_no_reset:
	
		call cs:game_drop		;call function to drop the characters
		call cs:game_crash		;call function to detect if there is a character crash
		call cs:display_play	;call function to update display
		
		mov al,game_speed		;mov the game_speed - no. of time of delay18 must be called
		call cs:delay_time		; in each loop and call delay_time to delay the required time
		
		game_32h_paue_loop:
		cmp play_pause,1				;check if the pause flag is up. If yes, it will
			jne game_32h_not_paused		;run an infinite loop here until the pause flag is down
				call cs:delay18
			jmp game_32h_paue_loop
		game_32h_not_paused:
		
		cmp play_life,0					;exit loop if play_life(no. of life left) is 0
			je game_32_mgame_end_loss
			
		cmp	play_quit,1					;exit loop if quit flag is up <- quit is executed
			je game_32_mgame_end		;jump to send "loss" signal to other comp
			
	jmp game_32_mgame_loop
	
	game_32_mgame_end_loss:
		mov to_be_send,0b6h			;send loss signal (0b6h) to the other computer
		call cs:send_byte			;call send_byte to send value in to_be_send
		
	game_32_mgame_end:
	ret
game_32_mgame endp

; ----------game_31_mgame_start ----------
; prepare for the actual  mulitplayer game
;game state: 31h
game_31_mgame_start	proc	far
	mov game_state,31h		;set game_state to 31h
	
	mov play_agree,0		;reset agreement flags
	mov agree_state,0
	
	call cs:display_clear	;clear play area
		
		mov _colour, 00001111b	;print msg waiting for handshake
		lea dx, msg_hand_start
		mov _text, dx
		mov _textlt, 24
		mov _x, 6
		mov _y, 10
	call cs:printline
		
	call cs:handshake_device_level 	;call handshaking
	call cs:set_comm		;call function to setup interrupt for comm 1
		
	call cs:display_clear	;clear play area
	
		mov _colour, 00001111b	;print msg handshaking complete
		lea dx, msg_hand_done
		mov _text, dx
		mov _textlt, 21
		mov _x, 7
		mov _y, 10
	call cs:printline
	
	mov al,8			;delay the screen to inform user 
	call cs:delay_time	;that handshaking is complete
		
	call cs:display_clear	;clear play area
	
		mov _colour, 00001111b	;print to ask to enter difficulty lvl
		lea dx, msg_diff
		mov _text, dx
		mov _textlt, 30
		mov _x, 4
		mov _y, 10
	call cs:printline
	
		mov _colour, 00001111b	;print the msg (0-9)
		lea dx, msg_09
		mov _text, dx
		mov _textlt, 5
		mov _x, 16
		mov _y, 11
	call cs:printline

		
	call cs:set_key	;setup interrupt for keyboard
	
	mgame_31_loop:			;call a delay until the agreement flag play_agree is set
		call cs:delay18		;signal end of setting lvl 
		cmp play_agree,1	;agreement on both sides
			je mgame_31_loop_end
		jmp mgame_31_loop
	mgame_31_loop_end:
		
	sub game_lvl,30h	;Convert game_lvl from ASCII to binary
	
	mov play_life,20		;reset all game variables and flags
	mov play_score,0
	mov play_drop_count,0
	mov play_add,0
	mov	play_pause,0
	mov play_win,0
	mov play_quit,0
	mov cheat_slow_count,0
	mov cheat_change_count,0
		
	xor bx,bx				;reset game arrays
	xor cx,cx
	mov cx,16
	clear_previous_mgame:
		mov	play_drop[bx],0
		mov	play_char[bx],0
		mov play_loc[bx],0
		inc bx
	loop clear_previous_mgame
		
	mov al,8			;delay sometime before the start of game
	call cs:delay_time
		
	call cs:mgame_set_lvl	;call function to set the game lvl
	call cs:rnd_char_all	;call function to randomize a set of characters
	call cs:display_play	;call function to update play area
	call cs:display_right	;call function to update scoreboard
	
	call cs:game_32_mgame	;call the actual game
	
	call cs:restore_key		;end of game; restore keyboard interrupts
		
	call cs:display_clear	; clear play area
	
		mov _colour, 00001100b	;print msg game over
		lea dx, msg_gameover
		mov _text, dx
		mov _textlt, 10
		mov _x, 12
		mov _y, 11
	call cs:printline
		
	cmp play_win,1					;check will win flag is set
	jne	mgame_31h_not_win			; if yes, print msg you are the winner
			mov _colour, 00001100b
			lea dx, msg_win
			mov _text, dx
			mov _textlt, 19
			mov _x, 8
			mov _y, 12
		call cs:printline
	mgame_31h_not_win:
		
	mov ax,play_score			;check if current score is higher than high score
	cmp ax,play_hi_score		;if yes, replace high score with current score
		jl m_not_hi_score
			mov play_hi_score,ax
	m_not_hi_score:
	
	call cs:display_right		;update scoreboard
		
	mov ah,0		;wait for any key press before continue
	int 16h
	
	call cs:restore_comm	;restore communication interrupt
	
	ret		;return back to main menu
game_31_mgame_start	endp

;////////////////////   Single player game procedures   ////////////////////

; ---------- game_22_sgame ----------
; actual Single Player game
;game state: 22h
game_22_sgame proc far
	mov game_state,22h	;Set game_state to 22h
	
	game_22_sgame_loop:
	
		cmp play_add,0				;section used to determine when to add a new charater
			jne game_add_no			;checks play_add and only add if it is 0
			call cs:game_add		;play_add will be inc 1 when it is in the loop
		game_add_no:				;play_add will be reset to 0 if it is equal to game_add
			inc play_add			;when play_add = game_add, meaing no. of "game_add" 
			mov al,play_add			;loop has passed
			cmp al,game_inc
				jl game_add_no_reset
					mov play_add,0
		game_add_no_reset:

		call cs:game_drop		;call function to drop the characters
		call cs:game_crash		;call function to detect if there is a character crash
		call cs:display_play	;call function to update display

		mov al,game_speed		;mov the game_speed - no. of time of delay18 must be called
		call cs:delay_time		; in each loop and call delay_time to delay the required time

		cmp play_life,0				;exit loop if play_life(no. of life left) is 0
			je game_22_sgame_end

		cmp	play_quit,1					;exit loop if quit flag is up <- quit is executed
			je game_22_sgame_end

	jmp game_22_sgame_loop
	
	game_22_sgame_end:
	ret
game_22_sgame endp

; ---------- game_21_sgame_start ----------
; prepare for the actual  single player game
;game state: 21h
game_21_sgame_start	proc far
	mov game_state,21h				;set game_state to 21h
	
	call cs:display_clear			;clear play area
	
		mov _colour, 00001111b		;print msg to ask to enter difficulty lvl
		lea dx, msg_diff
		mov _text, dx
		mov _textlt, 30
		mov _x, 4
		mov _y, 10
	call cs:printline
	
		mov _colour, 00001111b		;print msg (0-9)
		lea dx, msg_09
		mov _text, dx
		mov _textlt, 5
		mov _x, 16
		mov _y, 11
	call cs:printline
	
game_21_sgame_start_input:
	mov ah,0		;wait and get user input from keyboard
	int 16h
		
	cmp al,30h							;check if input is smaller than '0' in ASCII table
		jl game_21_sgame_start_input	;jump back to get the next keystroke if yes
	cmp al,39h							;check if input is greater than '9' in ASCII table
		jg game_21_sgame_start_input	;jump back to get the next keystroke if yes
		
		mov cl,al			;print the input character if it is valid
		mov ch,00001111b
		mov bh,14
		mov bl,18
	call cs:putchar

	sub al,30h			;Convert input from ASCII to binary
	mov	game_lvl,al		;store value in game_lvl
		
	mov al,8			;delay for a moment to show user the input value
	call cs:delay_time
		
	mov play_life,20		;reset all game variables and flags
	mov play_score,0
	mov play_drop_count,0
	mov play_add,0
	mov cheat_slow_count,0
	mov cheat_change_count,0
	mov play_quit,0
		
	xor bx,bx				;reset game arrays
	xor cx,cx
	mov cx,16
	clear_previous:
		mov	play_drop[bx],0
		mov	play_char[bx],0
		mov play_loc[bx],0
		inc bx
	loop clear_previous
		
	call cs:set_key			;call function to setup keyboard interrupt
	call cs:game_set_lvl	;call function to set the game lvl
	call cs:rnd_char_all	;call function to randomize a set of characters
	call cs:display_play	;call function to update play area
	call cs:display_right	;call function to update scoreboard
	
	call cs:game_22_sgame	;call actual game
	
	call cs:restore_key		;end of game; restore keyboard interrupts
		
	call cs:display_clear	; clear play area
	
		mov _colour, 00001100b	;print msg game over
		lea dx, msg_gameover
		mov _text, dx
		mov _textlt, 10
		mov _x, 12
		mov _x, 12
		mov _y, 11
	call cs:printline
		
	mov ax,play_score			;check if current score is higher than high score
	cmp ax,play_hi_score		;if yes, replace high score with current score
		jl not_hi_score
			mov play_hi_score,ax
	not_hi_score:
	
	call cs:display_right		;update scoreboard	
		
	mov ah,0		;wait for any key press before continue
	int 16h
		
	ret		;return back to main menu
game_21_sgame_start endp

;////////////////////   Main Menu   ////////////////////

; ---------- game_12_menu ----------
; main menu to show user what to enter for specific part of the program
;game_state: 12h
;output choose in al
game_12_menu	proc	far
	mov game_state,12h		;set game_state to 12h
	
	call cs:display_clear		;clear play area
	
		mov _colour, 00001111b	;print msg please select
		lea dx, msg_menu_pls
		mov _text, dx
		mov _textlt, 14
		mov _x, 5
		mov _y, 10
	call cs:printline
	

		lea dx, msg_menu_1		;print msg 1) single player
		mov _text, dx
		mov _textlt, 23
		mov _x, 7
		mov _y, 11
	call cs:printline
	
		lea dx, msg_menu_2		;print msg 2) multiplayer
		mov _text, dx
		mov _textlt, 22
		mov _x, 7
		mov _y, 12
	call cs:printline

		lea dx, msg_menu_3		;print msg 3)exit
		mov _text, dx
		mov _textlt, 9
		mov _x, 7
		mov _y, 13
	call cs:printline
	
	menu_12_wait:
	mov ah,0		;wait for user input before continuing
	int 16h
		
	cmp al,31h				;check if input is smaller than 1
		jl menu_12_wait		;repeat input if yes
	cmp al,33h				;check if input is greater than 3
		jg menu_12_wait		;reapeat input if yes

	ret			;return to jmp to specific part of the program
game_12_menu	endp

; ---------- game_11_welcome ----------
; print welcome msg and border and main layout
;game_state: 11h
game_11_welcome	proc	far
	mov game_state,11h		;set game_state to 11h
	
	call cs:display_border	;call function to print border
	call cs:display_right	;call function to print scoreboard
	
		mov _colour, 00001111b	;print welcome msg
		lea dx, msg_welcome
		mov _text, dx
		mov _textlt, 31
		mov _x, 3
		mov _y, 10
	call cs:printline
	
		lea dx, msg_press_key	;print press any key to continue
		mov _text, dx
		mov _textlt, 26
		mov _x, 6
		mov _y, 12
	call cs:printline
	
	mov ah,0	;wait for key press
	int 16h
	
	ret	;jump to main menu
game_11_welcome	endp

;%%%%%%%%%%%%%%%%%%  Keyboard and Comms  %%%%%%%%%%%%%%%%%% 
;interrupts ptach default interrupt to our own interrupts for keyboard and com port 1
;	- set and restore keyboard
;	-set and restore com port 1
;	-handshaking
;	- send data
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
; ---------- set_comm ----------
; patch interrupt 0ch (comm port) to our interrupt newint0ch
set_comm proc far
	
;Get oldint0 and store in oldint0
	mov     ax, 350ch   ;AH=35h, AL=Int 0ch
	int     21h			;call interrupt 21h,35 to get address in es:bx
	mov     word ptr oldint0c, bx	;store old int 0ch address
	mov     word ptr oldint0c+2, es

; Patch the interrupt vector table with the address of our ISR
	mov     dx, seg newint0c
	mov     ds, dx
	lea     dx, newint0c
	mov     ax, 250ch   	;AH=25, AL=Int 0ch.
	int     21h				;call interrupt 21h,25 to patch  address of int 0ch
	
	mov     ax, data		;Restore DS so it
	mov     ds, ax			; points back at data.

; Enabke IRQ 4 in PIC
	in      al, 21h         ;Read existing bits.
	and     al, 0efh        ;Turn on IRQ 4 (COM1).
	out     21h, al         ;Write result back to PIC.
			
	ret
set_comm	endp

; ---------- restore_comm ----------
; Restore the interrupt vector for int0ch, com 1
restore_comm proc far
	lds     dx, oldint0c
	mov     ax, 250ch	;AH=25, AL=Int 0ch.
	int     21h			;call interrupt 21h,25 to patch  address of int 0ch
	
	mov     ax, data	;Restore DS so it
	mov     ds, ax		; points back at data.
			
			ret
restore_comm	endp

; ---------- set_key ----------
; setup ISR for int9h, keyboard
set_key proc far

;Get oldint0 and store in oldint0
	mov     ax, 3509h   ;AH=35h, AL=Int 9.
	int     21h			;call interrupt 21h,35 to get address in es:bx
	mov     word ptr oldint9, bx	;store old int 9 address
	mov     word ptr oldint9+2, es

; Patch the interrupt vector table with the address of our ISR
	mov     dx, seg newint9
	mov     ds, dx
	lea     dx, newint9
	mov     ax, 2509h   ;AH=25, AL=Int 9.
	int     21h			;call interrupt 21h,25 to patch  address of int 9
	
	mov     ax, data	;Restore DS so it
	mov     ds, ax		; points back at Data.
			
	ret
set_key endp

; ---------- restore_key ----------
; Restore the interrupt vector for int9h, keyboard
restore_key proc far
	lds     dx, oldint9
	mov     ax, 2509h	;AH=25, AL=Int 9.
	int     21h			;call interrupt 21h,25 to patch  address of int 9
	
	mov     ax, data	;Restore DS so it
	mov     ds, ax		; points back at Data.

	ret
restore_key endp

; ---------- handshake_device_level ----------
; a complete handshaking of comm port
handshake_device_level  proc    far
	mov     ax, 00e3h		; 1 start bit, 1 stop bit, no parity bit
	mov 	dx, comm_port_set
	int     14h				; 8-bit data, 9600 baud rate               

	call    cs:dtr_init		; initialise dtr\ to logic 0 (active)
	call    cs:dsr_stats	; check dsr\ status
	call	cs:rts_init		;initalise rts device lvl
	
	ret
handshake_device_level  endp

; ---------- dtr_init ----------
; activate DTR
dtr_init	proc	far
	mov     dx, mcr	; set up modem control port address
	mov     al, 01h	; initialise dtr\ to logic 1
	out     dx, al          

	ret
dtr_init	endp

; ---------- dsr_stats ----------
; check DSR status
dsr_stats	proc	far
	mov     dx, msr	; set up modem status port address

again_dsr:
	in      al, dx	; check status of dsr
	test    al, 20h
	jz      again_dsr

	ret
dsr_stats	endp

; ---------- rts_init ----------
; activate RTS
rts_init	proc	far
	mov     dx, mcr	; set up modem control port address
	mov     al, 00001011b	; initialise rts\ to logic 1
	out     dx, al
	
	mov		dx,ier
	mov		al, 01h
	out		dx,al
	ret
rts_init	endp

; ---------- newint0c ----------
; interrupt to be run when int 0ch (com port 1) is called
newint0c proc far
	
	push ax		;save register
	push bx
	push cx
	push dx
	
	newint0c_start:
	mov dx,iir				;test if there is received data
	in al,dx				;end if there is none
	test al,1
	jnz newint0c_exit
	
	rx_on_byte:
	mov     dx, lsr			; set up line status port address

	wait_rx_byte:
	in      al, dx			; check if data ready bit 0 ready
	test    al, 01h
	jz      wait_rx_byte

	receive_rx_byte:
	mov     dx, rbr			; set up receiver buffer port address
	in      al, dx			; receive data through rx
	
	cmp game_state,31h		;check game state and call 31h if program is in that state
		jne not_31h_rec
			call cs:mgame_31h_rec
	not_31h_rec:
	
	cmp game_state,32h		;check game state and call 32h if program is in that state
		jne not_32h_rec
			call cs:mgame_32_int_rec
	not_32h_rec:
	
	jmp newint0c_start		;jmp back to check if ther is still data in buffer
	
	newint0c_exit:
	mov al,20h			;acknowledge interrupt received
	out 20h,al
	
	pop dx		;restore register
	pop cx
	pop bx
	pop ax
	
	iret
newint0c endp

; ---------- newint9 ----------
; interrupt to be run when int 09h (keyboard)  is called
newint9 proc far

	push ax		;save register
	push bx
	push cx
	push dx
	
	pushf                   ;Simulate an INT instruction by pushing
	call   cs:oldint9		; call the old interrupt once
	
	mov     ah, 1           ;Is a key available?
	int     16h
	jz      BufferIsClr     ;If not, Discontinue flushing
	mov     ah, 0           ;Flush this character from the
	int     16h             ; buffer and try again.
		
	cmp game_state,31h		;check game state and call 31h if program is in that state
		jne not_31h
			call cs:mgame_31h
	not_31h:
		
	cmp game_state,22h		;check game state and call 22h
	jne not_22h
	
		cmp ax,011bh			;check if esc is pressed
			jne no_esc_22h
				call cs:game_pause	;run game_pause if esc is pressed
		no_esc_22h:

		call cs:sgame_22_int	;call sgame_22_int if game is in state 22h and no esc is pressed
	not_22h:
		
	cmp game_state,32h		;check game state and call 32h
		jne not_32h
		
		cmp play_pause,1	;check if play_pause flag is up
			je not_32h		;disable keyboard if it is
			
			cmp ax,011bh		;check if esc is pressed
				jne no_esc_32h

					mov to_be_send,0b3h		;send pause signal to the other computer
					call cs:send_byte

					call cs:game_pause		;call game_pause
		no_esc_32h:
		
		call cs:mgame_32_int	;call sgame_22_int if game is in state 32h and no esc is pressed
	not_32h:
		
	BufferIsClr:
	
	pop dx		;restore register
	pop cx
	pop bx
	pop ax
	
	iret
newint9 endp

; ---------- send_byte ----------
; send value in to_be_send to the other computer
;input: to_be_send
send_byte proc far
	tx_on_byte:
		mov     dx, lsr                         ; set up line status port address
		
	wait_tx_byte:
		in      al, dx                          ; check if transmitter ready bit 5 ready
		test    al, 20h
		jz      wait_tx_byte

	send_tx_byte:
		mov     dx, thr                         ; set up transmitter holding port address
		mov     al, to_be_send                   ; send data through tx
		out     dx, al
		
		ret
send_byte endp

;%%%%%%%%%%%%%%%%%%  Delay  %%%%%%%%%%%%%%%%%% 
;Procdure called for delay
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

; ---------- delay_time ----------
; call delay18 in a specific no. of times
;input: al
delay_time proc far
	push cx			;save cx
	
		xor cx,cx	;setup counter
		mov cl,al
		
		delay_time_loop:	;call delay18 for no. of time in al
			call cs:delay18
		loop delay_time_loop
		
	pop cx			;restore cx
	
	ret
delay_time endp

; ---------- INITCYCLE ----------
; find timedvalue for use with delays
INITCYCLE	proc	far
main1:
	mov	ax, 40h
	mov	es, ax

	mov	ax, RTC

RTCmustChange:
	cmp	ax, RTC
	je	RTCmustChange

	xor	cx, cx
	mov	si, RTC
	mov	dx, PPI_B

TimeRTC:
	mov	bx, 10
DelayLp:
	in	al, dx
	dec	bx
	jne	DelayLP
	cmp	si, RTC
	loope	TimeRTC

	neg	cx
	mov	timedvalue, cx
	mov	ax, ds
	mov	es, ax

	ret
INITCYCLE	endp

; ---------- Delay18 ----------
; Delays for approx 1/18 seconds
Delay18		proc	far
	push	ds
	push	es
	push	ax
	push	bx
	push	cx
	push	dx
	push	si

	mov	ax, data
	mov	es, ax
	mov	ds, ax

	mov	cx, TimedValue
	mov	si, es:RTC2
	mov	dx, PPI_B

TimeRTC2:
	mov	bx, 10

DelayLp2:
	in	al, dx
	dec	bx
	jne	DelayLp2
	cmp	si, es:RTC2
	loope	TimeRTC2
	
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es
	pop	ds
	
	ret
Delay18	endp

;%%%%%%%%%%%%%%%%%%  Display  %%%%%%%%%%%%%%%%%% 
;Procdure to print box, border and text on screen 
;2 parts	-general drawing procedure
;		-specific drawing procedure to draw part of the game
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


;////////////////////   specific drawing procedures   ////////////////////

; ---------- display_play ----------
; draw out the actual game play area by getting data from the play_char, play_loc array
display_play proc far
	push ax			;save registers
	push bx
	push cx
	push dx
	
	call cs:display_clear	;clear play area
	
	xor bx, bx					;setup index counter
	display_play_loop:
		mov al, play_char[bx]	;copy char to al
		mov ah, play_loc[bx]	;opy the location of char to ah
		
		push bx				;save bx which is used for index
		
			add ah,2		;process height from play_loc to actual corrdinates according to screen
			mov bh,ah		

			add bl,bl		;process width to actual coordinates on the screen
			add bl,3
			
			mov cl,al		;move variables to register for putchar to be called
			mov ch,00001111b
			
			cmp	bh,19		;change color to red if the charater location is lower than 19
				jl	display_play_no_red
			mov ch,00001100b
		display_play_no_red:
		
			call cs:putchar	;call put char for char to be printed
			
		pop bx				;restore bx to be use as index
		
		inc bx				;inc bx and exit only when bx =16
		cmp bx,16
			jne display_play_loop
		
	pop dx			;restore registers
	pop cx
	pop bx
	pop ax
	
	ret
display_play endp

; ---------- display_play ----------
; clear to play area by printing a balkc box over it
display_clear proc far
	mov _colour, 00000000b	;setup all required settings to print black box over whole play area
	mov _x, 1
	mov _y, 0
	mov _ht, 23
	mov _lt, 35
call cs:printbox			;call printbox

	ret
display_clear endp

; ---------- display_border ----------
; print the border of the game
display_border proc far
	mov _char, 0B1h			;setup all required settings to print the border of the game
	mov _colour, 00000111b
	mov _x, 0
	mov _y, 0
	mov _ht, 24
	mov _lt, 36
call cs:printborder			;call print border
			
			ret
display_border endp

; ---------- display_right ----------
; print the scoreboard on the right side of the screen with updated scores and values
display_right	proc	far
	mov _colour, 01000000b		;print the bottom re box border
	mov _x, 41
	mov _y, 2
	mov _ht, 21
	mov _lt, 20
call cs:printbox

	mov _colour, 01110000b		;print grey box
	mov _x, 40
	mov _y, 1
	mov _ht, 21
	mov _lt, 20
call cs:printbox
	
	lea dx, msg_lives			;print msg lives left:
	mov _text, dx
	mov _textlt, 11
	mov _x, 42
	mov _y, 3
call cs:printline
			
	xor ax,ax
	mov al,play_life			;print the actual no. of lives left
	mov _colour, 01110000b
	mov _x, 48
	mov _y, 5
call cs:printnum
			
	mov _colour, 01111111b		;print msg level:
	lea dx, msg_lvl
	mov _text, dx
	mov _textlt, 6
	mov _x, 42
	mov _y, 7
call cs:printline

	xor ax,ax					;print the actual value of game level
	mov al,game_lvl
	mov _colour, 01110000b
	mov _x, 48
	mov _y, 9
call cs:printnum
			
	mov _colour, 01111111b		;print msh score:
	lea dx, msg_score
	mov _text, dx
	mov _textlt, 6
	mov _x, 42
	mov _y, 11
call cs:printline

xor ax,ax						;check that score is not negative
mov ax,play_score
test	ah,10000000b
	jz above_zero
		mov play_score,0		;reset score to 0 if score is negative
		xor ax,ax
above_zero:

	mov _colour, 01110000b		;print the actual value
	mov _x, 48
	mov _y, 13
call cs:printnum
			
	mov _colour, 01111111b		;print msg high_score
	lea dx, msg_hiscore
	mov _text, dx
	mov _textlt, 11
	mov _x, 42
	mov _y, 15
call cs:printline
			
	xor ax,ax					;print the actual high score
	mov ax,play_hi_score
	mov _colour, 01110000b
	mov _x, 48
	mov _y, 17
call cs:printnum
			
	ret
display_right	endp

;////////////////////   general drawing procedures   ////////////////////

; ---------- printnum ----------
; read in value from ax and print it on screen
;input: 	 ax  		value to be printed
;		_x  		x coordinate
;		_y  		y coordinate
;		_color 	color setting
printnum proc far
		PUSH    AX              ;save the registers
		PUSH    BX
		PUSH    CX
		PUSH    DX
		
		XOR     CX,CX
		MOV     BX,10D
@REPEAT2:
		XOR     DX,DX
		DIV     BX
		PUSH    DX
		INC     CX

		OR      AX,AX
		JNE     @REPEAT2
		MOV     AH,2
		
		mov 	bl, _x
		mov 	bh, _y		

@PRINT_LOOP:
		POP     AX                ;digit in DL
		push 	cx
		OR      AL,30H          ;convert to character
		mov 	ch, _colour
		mov		cl,al
		call 	cs:putchar
		inc 	bl
		pop 	cx
		LOOP    @PRINT_LOOP     ;loop until done

		POP     DX				;retrieve the registers
		POP     CX
		POP     BX
		POP     AX
		
		ret
printnum	endp

; ---------- printbox ----------
; print an area with same color and character
;input: 	_x  		x coordinate
;		_y  		y coordinate
;		_color 	color setting
;		_ht		height of box
;		_lt		length of box
;		_char	character to be printed
printbox proc far
  push ax
  push cx
  push dx
  push bx

	  mov cl, 0
	  mov ch, _colour
	  mov bl, _x
	  mov bh, _y	; this is the (actual coordinate - 1)
	  mov ah, _ht 	; ah is the height to be shaded

draw_bkg:
	  mov al, _lt	; al is the length to be shaded
	  mov bl, _x
	  add bh, 1

draw_bkgline:  
	  call putchar
	  inc bl
	  dec al
	  jne draw_bkgline

	  dec ah
	  jne draw_bkg  

  pop bx
  pop dx
  pop cx
  pop ax

  ret
printbox endp

; ---------- printborder ----------
; print an outline of an  area with same color and character
;input: 	_x  		x coordinate
;		_y  		y coordinate
;		_color 	color setting
;		_ht		height of border
;		_lt		length of border
;		_char	character to be printed
printborder proc far
  push ax
  push cx
  push dx
  push bx 

	  mov cl, _char
	  mov ch, _colour
	  mov bl, _x
	  mov bh, _y

	  mov al, _lt	; drawing the horizontal stroke right
draw_hr:  
	  call putchar
	  inc bl
	  dec al
	  jne draw_hr

	  mov al, _ht	; drawing the vertical stroke down
draw_vd:
	  call putchar
	  inc bh ; Move to next pixel
	  dec al
	  jne draw_vd

	  mov al, _lt	; drawing horizontal stroke left
draw_hl:  
	  call putchar
	  dec bl
	  dec al
	  jne draw_hl

	  mov al, _ht	; drawing the vertical stroke up
draw_vu:
	  call putchar
	  dec bh ; Move to next pixel
	  dec al
	  jne draw_vu

  pop bx
  pop dx
  pop cx
  pop ax

  ret
printborder endp

; ---------- printborder ----------
; print an outline of an  area with same color and character
;input: 	_x  		x coordinate
;		_y  		y coordinate
;		_color 	color setting
;		_text		address of msg
;		_textlt	length of msg
printline proc far
  push ax
  push cx
  push dx
  push bx

	mov ax,data 	; set up ds as the segment for data
	mov es,ax 		; put this in es

	mov bp, _text 	; ES:BP points to message
	mov ah,13h 		; function 13 - write string
	mov al,01h 		; attrib in bl,move cursor
	xor bh,bh 		; video page 0
	mov bl,_colour 	; attribute - magenta
	mov cx,_textlt	; length of string
	mov dh,_y 		; row to put string
	mov dl,_x 		; column to put string
	int 10h 		; call BIOS service

  pop bx
  pop dx
  pop cx
  pop ax

ret
printline endp

; ---------- PutChar ----------
; print a character on screen
;input: 	cl  		Character to be printed
;		ch  		Color setting
;		bl 		x coordinate
;		bh		y coordinate
PutChar PROC
	PUSH ES 
	PUSH AX 
	PUSH DX 
	PUSH BX 
 
		MOV AX, 0B800h 
		MOV ES, AX ; Segment Address of the Text Mode Display Buffer 
		 
		MOV AL, BH 
		XOR AH, AH 
		MOV DL, 160 
		MUL DL ; Row * 160 
		XOR BH, BH 
		SHL BX, 1 ; CX = Column *2 
		ADD BX, AX ; Row*160+Column*2 
		MOV ES:[BX], CX 
	 
	POP BX 
	POP DX 
	POP AX 
	POP ES 
RET 
PutChar ENDP 

;%%%%%%%%%%%%%%%%%%  Random functions  %%%%%%%%%%%%%%%%%% 
;procedures to generate random numbers and characters
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; ---------- rnd_num ----------
; generate a random number with range (0  to dl -1) and ouput in al
;input:	dl	range of random number required
;output	al	output of random number
rnd_num 	proc	far

    in al, 40h				;get random value from mirco timer
	add dice_tx,al			;add value to dice_tx

	mov	ax, RTC				; saves current RTC value into ax
	xor ah,ah				;take only the LSB part of the RTC
	add dice_tx,al			;sum to dice_tx
	
	mov al,dice_tx			;get the new dice_tx value

	div dl					;divide dice_tx with the required range of random no.
	xchg al,ah				;and copy reminder to al
	xor ah,ah
	
	ret
rnd_num	endp

; ---------- rnd_char_all ----------
; regenerate a new array of character in play_char
rnd_char_all 	proc	far
	push ax		;save registers
	push bx
	push cx
	push dx
	
	xor bx,bx
	rnd_char_loop:		;run loop for 16 times
		call cs:rnd_char
		inc bx
	cmp bx,16
		jne rnd_char_loop
	
	pop dx		;restore registers
	pop cx
	pop bx
	pop ax
	
	ret
rnd_char_all	endp

; ---------- rnd_char ----------
; regenerate a new character in play_char at position bx
rnd_char 	proc	far
	push ax		;save registers
	push bx
	push cx
	push dx
	
	mov dl,26		;generate a random nio. from 0-25
	call cs:rnd_num
	
	add al,61h		;add 61h to convert generated no. to ASCII
	mov play_char[bx],al
		
	pop dx		;restore registers
	pop cx
	pop bx
	pop ax	
	
	ret
rnd_char	endp

;%%%%%%%%%%%%%%%%%%  interrupt functions  %%%%%%%%%%%%%%%%%% 
;procedures to be ran when interrupt at specific game_state is called
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; ---------- sgame_22_int ----------
; procedure ran when int 9 is called and game_state at 22h
;input		al		character inputed from keyboard
sgame_22_int	proc	far
	xor cx,cx
	xor bx,bx
	xor dx,dx
	mov cx,16
	sgame_22_int_compare:
		cmp play_drop[bx],0		;first detect if char is dropping
			je sgame_22_int_no	;if character then run second loop
			cmp play_char[bx],al		;second to detect if char is same
				jne sgame_22_int_no		;if different, jump back to first loop and continue with next character
					mov dh,play_loc[bx]	;if same, copy the play_loc to dh and mov the index no. to bl
					mov dl,bl			;after which, enter the third loop
					sgame_22_int_great:
					cmp bx,15	;check if index is at the end of the array
						je	sgame_22_int_del
							inc bx
							cmp	play_char[bx],al	;check to find if there is the same character
								jne sgame_22_int_great
									cmp play_loc[bx],dh	;if same character is found
										jl sgame_22_int_great	;compare location if location is larger,
											mov dh,play_loc[bx]	;update dx with location and index no.
											mov dl,bl
								jmp sgame_22_int_great			;loop until the end of array

		sgame_22_int_no:
		inc bx
	loop sgame_22_int_compare

	sub	play_score,5	;deduct score when there is no match with the input character within the whole array
	jmp sgame_22_int_end
		
	sgame_22_int_del:		;action done when a matching character is detected
	xor bx,bx
	mov bl,dl
	
	push ax
	inc play_kills			;increase score, and update kill no.
	
	xor ax,ax
	mov ax,22
	sub al,play_loc[bx]
	add al,game_lvl
	add al,game_lvl
	add al,game_lvl
	add al,game_lvl
	add	play_score,ax
	pop ax
	
	mov	play_loc[bx],0		;reset drop location and regenerate character
	mov play_drop[bx],0
	call cs:rnd_char
	dec play_drop_count
		
	cmp al,77h				;compare to see if character killed is W if, yes apply a delay
		jne sgame_22_not_w
			call cs:display_play	;update display
			call cs:display_right
				
			mov al,20			;apply delay
			call cs:delay_time
	sgame_22_not_w:
		
	cmp play_kills,12		;compare if no. of kill match 12. if yes, increase level
		jne sgame_22_int_end
			mov play_kills,0	;reset kill counter
	
			inc game_lvl
			call cs:game_set_lvl	;call to update speed and drop rate
	sgame_22_int_end:
	
	xor bx,bx					;check for cheat "slow"
	mov bl,cheat_slow_count
	cmp	cheat_slow[bl],al
		je	cheat_slow_check
			mov cheat_slow_count,0	;if character entered does not tally reset cheat_slow_count
			jmp cheat_slow_end
			
	cheat_slow_check:
	inc cheat_slow_count	;if character entered tally, inc cheat_slow_count
	inc bl
	cmp	cheat_slow[bl],'$'	;compare to see if cheat_slow_count is fully entered and enable cheat
		jne cheat_slow_end
			call cs:display_play	;update display
			call cs:display_right
				
			mov al,50				;call for a delay for cheat
			call cs:delay_time		
			mov cheat_slow_count,0	;reset cheat counter
	cheat_slow_end:

	xor bx,bx					;check for cheat "change"
	mov bl,cheat_change_count
	cmp	cheat_change[bl],al
		je	cheat_change_check
			mov cheat_change_count,0	;if character entered does not tally reset cheat_change_count
			jmp cheat_change_end
			
	cheat_change_check:
	inc cheat_change_count	;if character entered tally, inc cheat_slow_count
	inc bl
	cmp	cheat_change[bl],'$'	;compare to see if cheat_change_count is fully entered and enable cheat
		jne cheat_change_end
			call cs:rnd_char_all	;change all characters
			
			call cs:display_play	;update display
			call cs:display_right
	cheat_change_end:
	
	call cs:display_play	;update display
	call cs:display_right
	
	ret
sgame_22_int	endp

; ---------- game_pause ----------
; procedure ran to pause the game and exit to main menu if e is entered
game_pause proc far
	call cs:restore_key		;patch interrupt vector so that keystrokes will be ignored
		
		mov _colour, 00001111b	;print msg game paused
		lea dx, msg_paused
		mov _text, dx
		mov _textlt, 29
		mov _x, 4
		mov _y, 11
	call cs:printline
		
	mov ah,0		;wait for user input
	int 16h
	
	cmp al,65h		;if user input is e
		jne no_quit
			mov play_quit,1	;set quit flag
	
			cmp game_state,32h		;if game is game_state 32h (multiplayer)
				jne sgame_pause_no_send
				
					mov to_be_send,0b6h	;send quit signal to the other comp
					call cs:send_byte
					
			sgame_pause_no_send:

	no_quit:					;if other keys are pressed
		call cs:display_play	;update display
		call cs:set_key			;patch back interrupt

	cmp game_state,32h			;if game _state 32h
		jne sgame_pause_exit
		
			mov to_be_send,0b4h	;send resume signal to the other comp
			call cs:send_byte
	
	sgame_pause_exit:
	ret
game_pause	endp

; ---------- mgame_31h ----------
; procedure ran when game is in state 31h, mgame_startand a key is pressed
;input		al		character inputed from keyboard
mgame_31h proc far
	cmp al,79h
		je mgame_31h_is_y	;if key is y? jmp to y section

	cmp al,30h			;if key is not 0-9? jmp to end
	jl mgame_31h_end
	cmp al,39h
	jg mgame_31h_end
		
		mov cl,al			;display input char (0-9) on screen
		mov ch,00001111b
		mov bh,14
		mov bl,18
	call cs:putchar
	
	mov game_lvl,al			;send inputed value over to the other comp.
	mov to_be_send,al
	call cs:send_byte
	
	mov agree_state,0		;update agreemetn flag
	jmp mgame_31h_end
		
	mgame_31h_is_y:			;if input is y
	cmp agree_state,1		;check if agreement flag is up
		jne mgame_31h_end
	mov play_agree,1		;update play_agree flag to show that the player agree to play at that level
	
	mov to_be_send,0a1h		;send signal to the other comp. to show that the player agree
	call cs:send_byte
		
	mgame_31h_end:
	ret
mgame_31h endp

; ---------- mgame_31h_rec ----------
; procedure ran when game is in state 31h, mgame_startand a signal from comm is detected
;input		al		character inputed from comm port
mgame_31h_rec proc far
	cmp al,0a1h			;if 0a1h is received
		je mgame_31h_rec_agree
		
	cmp al,30h			;if received is outside of 0-9
		jl mgame_31h_rec_end
	cmp al,39h
		jg mgame_31h_rec_end
		
	mov game_lvl,al		;update game_level with current one
	
	call cs:display_clear
	
		mov _colour, 00001111b	;print msg would you like to play at level:
		lea dx, msg_liketo
		mov _text, dx
		mov _textlt, 32
		mov _x, 2
		mov _y, 10
	call cs:printline

		mov _colour, 00001111b	;print msg press y to accept
		lea dx, msg_y
		mov _text, dx
		mov _textlt, 18
		mov _x, 9
		mov _y, 11
	call cs:printline
		
		mov cl,al				;print current received character onto screen
		mov ch,00001111b
		mov bh,14
		mov bl,18
	call cs:putchar
		
	mov agree_state,1			;change agreement flag
	
	jmp mgame_31h_rec_end
		
	mgame_31h_rec_agree:
		mov play_agree,1		;change play_agree flag if signal is received
		
	mgame_31h_rec_end:
	ret
mgame_31h_rec endp

; ---------- mgame_32_int ----------
; procedure ran when int 9 is called and game_state at 32h
;input		al		character inputed from keyboard
mgame_32_int	proc	far

	xor cx,cx
	xor bx,bx
	xor dx,dx
	mov cx,16
	mgame_32_int_compare:
		cmp play_drop[bx],0		;first detect if char is dropping
			je mgame_32_int_no	;if character then run second loop
			cmp play_char[bx],al		;second to detect if char is same
				jne mgame_32_int_no		;if different, jump back to first loop and continue with next character
					mov dh,play_loc[bx]	;if same, copy the play_loc to dh and mov the index no. to bl
					mov dl,bl			;after which, enter the third loop
					mgame_32_int_great:
					cmp bx,15					;check if index is at the end of the array
						je	mgame_32_int_del	
							inc bx						
							cmp	play_char[bx],al		;check to find if there is the same character
								jne mgame_32_int_great
									cmp play_loc[bx],dh		;if same character is found
										jl mgame_32_int_great	;compare location if location is larger,
											mov dh,play_loc[bx]	;update dx with location and index no.
											mov dl,bl
								jmp mgame_32_int_great			;loop until the end of array

		mgame_32_int_no:
		inc bx
	loop mgame_32_int_compare
	
		sub	play_score,5	;deduct score when there is no match with the input character within the whole array
		jmp mgame_32_int_end
		
	mgame_32_int_del:		;action done when a matching character is detected
	xor bx,bx
	mov bl,dl
	
	push ax
	inc play_kills			;increase score, and update kill no.
	
	xor ax,ax
	mov ax,22
	sub al,play_loc[bx]
	add al,game_lvl
	add al,game_lvl
	add al,game_lvl
	add al,game_lvl
	add	play_score,ax
	pop ax
	
	mov	play_loc[bx],0		;reset drop location and regenerate character
	mov play_drop[bx],0
	call cs:rnd_char
	dec play_drop_count
		
	mov	to_be_send,0b1h		;send signal to the other computer to add a drop
	call cs:send_byte
		
	cmp al,77h				;compare to see if character killed is W if, yes apply a delay
		jne mgame_32_not_w
			call cs:display_play	;update display
			call cs:display_right
			
			mov al,20			;apply delay
			call cs:delay_time
	mgame_32_not_w:
		
	cmp play_kills,12		;compare if no. of kill match 12. if yes, increase level
		jne mgame_32_int_end
			mov play_kills,0	;reset kill counter
			
			inc game_lvl
			call cs:mgame_set_lvl	;call to update speed and drop rate
				
			mov	to_be_send,0b2h	;send signal to level up to next computer
			call cs:send_byte
	mgame_32_int_end:
	
	xor bx,bx					;check for cheat "slow"
	mov bl,cheat_slow_count
	cmp	cheat_slow[bl],al
		je	m_cheat_slow_check
			mov cheat_slow_count,0	;if character entered does not tally reset cheat_slow_count
			jmp m_cheat_slow_end
			
	m_cheat_slow_check:
		inc cheat_slow_count	;if character entered tally, inc cheat_slow_count
		inc bl
		cmp	cheat_slow[bl],'$'	;compare to see if cheat_slow_count is fully entered and enable cheat
			jne m_cheat_slow_end
				call cs:display_play	;update display
				call cs:display_right
				
				mov al,50				;call for a delay for cheat
				call cs:delay_time		
				mov cheat_slow_count,0	;reset cheat counter
	m_cheat_slow_end:

	xor bx,bx					;check for cheat "change"
	mov bl,cheat_change_count
	cmp	cheat_change[bl],al
		je	m_cheat_change_check
			mov cheat_change_count,0	;if character entered does not tally reset cheat_change_count
			jmp m_cheat_change_end
			
	m_cheat_change_check:
	inc cheat_change_count	;if character entered tally, inc cheat_slow_count
	inc bl
	cmp	cheat_change[bl],'$'	;compare to see if cheat_change_count is fully entered and enable cheat
		jne m_cheat_change_end
			mov to_be_send,0b7h		;send signal to the other comp
			call cs:send_byte
	m_cheat_change_end:
	
	call cs:display_play	;update display
	call cs:display_right
	
	ret
mgame_32_int	endp

; ---------- mgame_32_int_rec ----------
; procedure ran when int 0ch, comm port 1 is called and game_state at 32h
;input		al		character inputed from comm port
mgame_32_int_rec	proc	far

	cmp al,0b1h					;if 0b1h is received add game character
		jne mgame_32_no_add
			call cs:game_add
	mgame_32_no_add:

	cmp al,0b2h					;if 0b2h is received perform a lvl up
		jne mgame_32_lvl_up
			mov play_kills,0
			inc game_lvl
			call cs:mgame_set_lvl
			call cs:display_right
	mgame_32_lvl_up:

	cmp al,0b3h					;if 0b3h is received pause game
		jne mgame_32_pause
			
			mov _colour, 00001111b	;print line game paused
			lea dx, msg_paused2
			mov _text, dx
			mov _textlt, 12
			mov _x, 13
			mov _y, 11
		call cs:printline
			
			mov play_pause,1	;setup pause game flag
		mgame_32_pause:

	cmp al,0b4h					;if 0b4h is received reseume game
		 jne mgame_32_resume
			mov play_pause,0	;set pause flag to 0
	 mgame_32_resume:
	
	 cmp al,0b6h				;if 0b6h is received quit game and set win flag to 1
		jne mgame_32_win
			mov play_win,1
			mov play_quit,1
			mov play_pause,0	;set pause flag to 0
	mgame_32_win:
	
	 cmp al,0b7h				;if 0b7h is received randomize char and update dispaly
		jne mgame_32_exit
			call cs:rnd_char_all	;change all characters
			call cs:display_play	;update display
			call cs:display_right
			
	mgame_32_exit:
	ret
mgame_32_int_rec endp
;=======================================================================================================================
CODE ENDS
END start ; This indicates the entry point of your program.
;=======================================================================================================================