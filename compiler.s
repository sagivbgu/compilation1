%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_RATIONAL 3
%define T_FLOAT 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10

%define TYPE_SIZE 1
%define WORD_SIZE 8
	
%define KB(n) n*1024
%define MB(n) 1024*KB(n)
%define GB(n) 1024*MB(n)


%macro SKIP_TYPE_TAG 2
	mov %1, qword [%2+TYPE_SIZE]	
%endmacro	

%define NUMERATOR SKIP_TYPE_TAG

%macro DENOMINATOR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%macro CHAR_VAL 2
	movzx %1, byte [%2+TYPE_SIZE]
%endmacro

%define FLOAT_VAL SKIP_TYPE_TAG

%define STRING_LENGTH SKIP_TYPE_TAG

%define SYMBOL_VAL SKIP_TYPE_TAG

%macro STRING_ELEMENTS 2
	lea %1, [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%define CAR SKIP_TYPE_TAG

%macro CDR 2
	mov %1, qword [%2+TYPE_SIZE+WORD_SIZE]
%endmacro

%define CLOSURE_ENV CAR

%define CLOSURE_CODE CDR

%define PVAR(n) qword [rbp+(4+n)*WORD_SIZE]

; returns %2 allocated bytes in register %1
; Supports using with %1 = %2
%macro MALLOC 2
	add qword [malloc_pointer], %2
	push %2
	mov %1, qword [malloc_pointer]
	sub %1, [rsp]
	add rsp, 8
%endmacro
	
; Creates a short SOB with the
; value %2
; Returns the result in register %1
%macro MAKE_CHAR_VALUE 2
	MALLOC %1, 1+TYPE_SIZE
	mov byte [%1], T_CHAR
	mov byte [%1+TYPE_SIZE], %2
%endmacro

; Creates a long SOB with the
; value %2 and type %3.
; Returns the result in register %1
%macro MAKE_LONG_VALUE 3
	MALLOC %1, TYPE_SIZE+WORD_SIZE
	mov byte [%1], %3
	mov qword [%1+TYPE_SIZE], %2
%endmacro

%define MAKE_FLOAT(r,val) MAKE_LONG_VALUE r, val, T_FLOAT
%define MAKE_CHAR(r,val) MAKE_CHAR_VALUE r, val

; Create a string of length %2
; from char %3.
; Stores result in register %1
%macro MAKE_STRING 3
	lea %1, [%2+WORD_SIZE+TYPE_SIZE]
	MALLOC %1, %1
	mov byte [%1], T_STRING
	mov qword [%1+TYPE_SIZE], %2
	push rcx
	add %1,WORD_SIZE+TYPE_SIZE
	mov rcx, %2
	cmp rcx, 0
%%str_loop:
	jz %%str_loop_end
	dec rcx
	mov byte [%1+rcx], %3
	jmp %%str_loop
%%str_loop_end:
	pop rcx
	sub %1, WORD_SIZE+TYPE_SIZE
%endmacro

;;; Creates a SOB with tag %2 
;;; from two pointers %3 and %4
;;; Stores result in register %1
%macro MAKE_TWO_WORDS 4 
        MALLOC %1, TYPE_SIZE+WORD_SIZE*2
        mov byte [%1], %2
        mov qword [%1+TYPE_SIZE], %3
        mov qword [%1+TYPE_SIZE+WORD_SIZE], %4
%endmacro

%macro MAKE_WORDS_LIT 3
	db %1
        dq %2
        dq %3
%endmacro

; Make a literal of type %1
; followed by the definition %2
%macro MAKE_LITERAL 2
	db %1
	%2
%endmacro

%define MAKE_LITERAL_CHAR(val) \
	MAKE_LITERAL T_CHAR, db val

%define MAKE_LITERAL_FLOAT(val) \
	MAKE_LITERAL T_FLOAT, dq val

%macro MAKE_LITERAL_STRING 1
	db T_STRING
	dq (%%end_str- %%str)
%%str:
	db %1
%%end_str:
%endmacro

%define MAKE_LITERAL_SYMBOL(val) \
	MAKE_LITERAL T_SYMBOL, dq val

%define MAKE_RATIONAL(r, num, den) \
	MAKE_TWO_WORDS r, T_RATIONAL, num, den

%define MAKE_LITERAL_RATIONAL(num, den) \
	MAKE_WORDS_LIT T_RATIONAL, num, den
	
%define MAKE_PAIR(r, car, cdr) \
        MAKE_TWO_WORDS r, T_PAIR, car, cdr

%define MAKE_LITERAL_PAIR(car, cdr) \
        MAKE_WORDS_LIT T_PAIR, car, cdr

%define MAKE_CLOSURE(r, env, body) \
        MAKE_TWO_WORDS r, T_CLOSURE, env, body

;;; Macros and routines for Lexical Environment

; MAKE_LIST_HEAD %1:register %2:length %3:head_pointer 
; address will be stored in register
; [LEN | MAJORS_HEAD_P]
; used for begining of the list of major lists
%macro MAKE_ENV_HEAD 3
		MALLOC %1, WORD_SIZE*2
		mov qword [%1], %2
		mov qword [%1+WORD_SIZE], %3
%endmacro

%define ENV_STACK_POSITION rbp+8*2
%define ARGS_COUNT_POSITION rbp+8*3

; EXTEND_ENV
; address of new env will be stored in RAX
%macro EXTEND_ENV_OLD 1
	; I AM RUNNING OVER RAX
	; because at the end of this macro i'm replacing it either way
	mov rax, qword [ENV_STACK_POSITION]
	; now in rax there's the length of the current env or SOB_NIL_ADDRESS
	cmp rax, SOB_NIL_ADDRESS
	jne %%extend
	; if it's equal, we are at the first level
	; so create an empty list:
	; 	address will be in rax as guranteed
	;	length = 0
	;	head_p = NIL
	
	MAKE_ENV_HEAD rax, 0, SOB_NIL_ADDRESS
	jmp %%end
%%extend:
	; store registers for personal use
	push rbx
	push rcx
	push rdx
	; get the length of the current env
	mov rcx, qword [ENV_STACK_POSITION]
	; rax = nothing
	; rbx = nothing
	; rcx = length of env
	; rdx = nothing

	; copy value to rax
	mov rax, rcx
	; rax = length of env
	; rbx = nothing
	; rcx = length of env
	; rdx = nothing

	; get the address of the current major list
	mov rbx, qword [ENV_STACK_POSITION]
	mov rbx, qword [rbx + WORD_SIZE]

	; rax = length of env
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; increment the length of env by 1
	inc rax
	; rax = length of ext_env = length of env + 1 
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; calculate the number of bytes to allocate
	mov rdx, WORD_SIZE
	imul rdx
	; rax = num of bytes to alloc for ext_env major list = length of ext_env * WORD_SIZE
	; rbx = address of env major list
	; rcx = length of env
	; rdx = 8

	; allocate the ext_env major list
	MALLOC rax, rax
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; LOOP
	cmp rcx, 0
%%extend_loop:
	jz %%extend_loop_end
	mov rdx, rcx
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env
	; rdx = length of env

	dec rcx
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env - 1
	; rdx = length of env

	; annotate rcx = n, so rdx = n + 1
	; need to do: rax + WORD_SIZE * (n + 1) <- rbx + WORD_SIZE * n
	; DOING : mov qword [rax + rdx*WORD_SIZE], qword [rbx + rcx*WORD_SIZE]
	push r8
	mov r8, qword [rbx + rcx*WORD_SIZE]
	mov qword [rax + rdx*WORD_SIZE], r8
	pop r8
	jmp %%extend_loop

%%extend_loop_end:
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = 0
	; rdx = 1

	; copy value of rax
	mov rbx, rax
	; rax = address of ext_env major list
	; rbx = address of ext_env major list
	; rcx = 0
	; rdx = 1

	; get the args_count from stack
	mov rcx, qword [ARGS_COUNT_POSITION]
	; rax = address of ext_env major list
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 1

	; calculate the number of bytes to allocate for extenv[0]
	mov rax, rcx
	mov rdx, WORD_SIZE
	imul rdx
	; rax = number of bytes to allocate for extenv[0]
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 8

	; allocate extenv[0] rib
	MALLOC rax, rax
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 1

	; link address of extenv[0] into ext_env major list in position 0
	mov qword [rbx], rax
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = args number
	; rdx = 1

	; COPY PARAMS LOOP
	cmp rcx, 0
%%copy_loop:
	jz %%copy_loop_end

	dec rcx
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = args number - 1
	; rdx = 1

	; annotate rcx = n
	; need to do rax + WORD_SIZE * n <- PVAR(n)
	push r8
	mov r8, PVAR(rcx)
	mov qword [rax + WORD_SIZE*rcx], r8
	pop r8
	
	jmp %%copy_loop
%%copy_loop_end:
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = 0
	; rdx = 1

	; restore the env length
	; get the length of the current env
	mov rcx, qword [ENV_STACK_POSITION]
	inc rcx
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = length of ext_env
	; rdx = 1

	; create the extended env list head, address will be stored in rax
	MAKE_ENV_HEAD rax, rcx, rbx

	; we used these regs at the begining of %%extend, so pop them back
	pop rdx
	pop rcx
	pop rbx
%%end:
	; address is in RAX as expected
%endmacro









%macro EXTEND_ENV 1
	; I AM RUNNING OVER RAX
	; because at the end of this macro i'm replacing it either way
	
	mov rax, qword [ENV_STACK_POSITION]
	; rax = the address of the LIST_HEAD of current env or SOB_NIL_ADDRESS
	cmp rax, SOB_NIL_ADDRESS
	
	jne %%extend
	; if it's equal, we are at the first level
	; so create an empty list:
	; 	address will be in rax as guranteed
	;	length = 0
	;	head_p = NIL
	MAKE_ENV_HEAD rax, 0, SOB_NIL_ADDRESS
	jmp %%end
%%extend:
	; rax = the address of the LIST_HEAD of current env 

	; store registers for personal use
	push rbx
	push rcx
	push rdx
	; get the length of the current env
	; PROBLEM HERE: rcx is not getting the length
	; All these didn't work:
	; 	1)
	; 	mov rcx, qword [ENV_STACK_POSITION]
	; 	2)
	; 	lea rdx, [ENV_STACK_POSITION]
	; 	mov rcx, qword [rdx]
	; 
	; TEMPORARY - put correct value - and it works!
	;mov rcx, 1
	mov rcx, qword [rax]
	
	; rax = the address of the LIST_HEAD of current env 
	; rbx = nothing
	; rcx = length of env
	; rdx = nothing

	; copy value to rax
	mov rax, rcx
	; rax = length of env
	; rbx = nothing
	; rcx = length of env
	; rdx = nothing

	; get the address of the current major list
	mov rbx, qword [ENV_STACK_POSITION]
	mov rbx, qword [rbx + WORD_SIZE]
	
	; rax = length of env
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; increment the length of env by 1
	inc rax
	; rax = length of ext_env = length of env + 1 
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; calculate the number of bytes to allocate
	mov rdx, WORD_SIZE
	imul rdx
	; rax = num of bytes to alloc for ext_env major list = length of ext_env * WORD_SIZE
	; rbx = address of env major list
	; rcx = length of env
	; rdx = 8

	; allocate the ext_env major list
	MALLOC rax, rax
	
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env
	; rdx = nothing

	; LOOP
	cmp rcx, 0
%%extend_loop:
	jz %%extend_loop_end
	mov rdx, rcx
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env
	; rdx = length of env

	dec rcx
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = length of env - 1
	; rdx = length of env

	; annotate rcx = n, so rdx = n + 1
	; need to do: rax + WORD_SIZE * (n + 1) <- rbx + WORD_SIZE * n
	; DOING : mov qword [rax + rdx*WORD_SIZE], qword [rbx + rcx*WORD_SIZE]
	push r8
	push r9
	;;; SEG FAULT IN THESE LINES --> FIXED
	; mov r8, qword [rbx + rcx*WORD_SIZE]
	; mov qword [rax + rdx*WORD_SIZE], r8

	lea r8, [rax + rdx*WORD_SIZE]
	lea r9, [rbx + rcx*WORD_SIZE]
	mov r8, r9

	pop r9
	pop r8
	jmp %%extend_loop

%%extend_loop_end:
	; rax = address of ext_env major list
	; rbx = address of env major list
	; rcx = 0
	; rdx = 1

	; copy value of rax
	mov rbx, rax
	; rax = address of ext_env major list
	; rbx = address of ext_env major list
	; rcx = 0
	; rdx = 1

	; get the args_count from stack
	mov rcx, qword [ARGS_COUNT_POSITION]
	; rax = address of ext_env major list
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 1

	; calculate the number of bytes to allocate for extenv[0]
	mov rax, rcx
	mov rdx, WORD_SIZE
	imul rdx
	; rax = number of bytes to allocate for extenv[0]
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 8

	; allocate extenv[0] rib
	MALLOC rax, rax
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list
	; rcx = args number
	; rdx = 1

	; link address of extenv[0] into ext_env major list in position 0
	mov qword [rbx], rax
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = args number
	; rdx = 1

	; COPY PARAMS LOOP
	cmp rcx, 0
%%copy_loop:
	jz %%copy_loop_end

	dec rcx
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = args number - 1
	; rdx = 1

	; annotate rcx = n
	; need to do rax + WORD_SIZE * n <- PVAR(n)
	push r8
	mov r8, PVAR(rcx)
	mov qword [rax + WORD_SIZE*rcx], r8
	pop r8
	
	jmp %%copy_loop
%%copy_loop_end:
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = 0
	; rdx = 1

	; restore the env length
	; get the length of the current env
	mov rcx, qword [ENV_STACK_POSITION]
	inc rcx
	; rax = address of extenv[0] rib
	; rbx = address of ext_env major list (now with all ribs linked)
	; rcx = length of ext_env
	; rdx = 1

	; create the extended env list head, address will be stored in rax
	MAKE_ENV_HEAD rax, rcx, rbx

	; we used these regs at the begining of %%extend, so pop them back
	pop rdx
	pop rcx
	pop rbx
%%end:
	; address is in RAX as expected
%endmacro






;;; Macros and routines for printing Scheme OBjects to STDOUT
%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32
%define CHAR_DOUBLEQUOTE 34
%define CHAR_BACKSLASH 92
	
extern printf, malloc
global write_sob, write_sob_if_not_void
	
write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, qword 0
	mov rdi, .undefined
	call printf

	pop rbp
	ret

section .data
.undefined:
	db "#<undefined>", 0

write_sob_rational:
	push rbp
	mov rbp, rsp

	mov rdx, rsi
	NUMERATOR rsi, rdx
	DENOMINATOR rdx, rdx
	
	cmp rdx, 1
	jne .print_fraction

	mov rdi, .int_format_string
	jmp .print

.print_fraction:
	mov rdi, .frac_format_string

.print:	
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.int_format_string:
	db "%ld", 0
.frac_format_string:
	db "%ld/%ld", 0

write_sob_float:
	push rbp
	mov rbp, rsp

	FLOAT_VAL rsi, rsi
	movq xmm0, rsi
	mov rdi, .float_format_string
	mov rax, 1

	;; printf-ing floats (among other things) requires the stack be 16-byte aligned
	;; so align the stack *downwards* (take up some extra space) if needed before
	;; calling printf for floats
	and rsp, -16 
	call printf

	;; move the stack back to the way it was, cause we messed it up in order to
	;; call printf.
	;; Note that the `leave` instruction does exactly this (reset the stack and pop
	;; rbp). The instructions are explicitly layed out here for clarity.
	mov rsp, rbp
	pop rbp
	ret
	
section .data
.float_format_string:
	db "%f", 0		

write_sob_char:
	push rbp
	mov rbp, rsp

	CHAR_VAL rsi, rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%02x", 0
.regular:
	db "#\%c", 0

write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	pop rbp
	ret

section .data
.void:
	db "#<void>", 0
	
write_sob_bool:
	push rbp
	mov rbp, rsp

	cmp word [rsi], word T_BOOL
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	pop rbp
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	pop rbp
	ret

section .data
.nil:
	db "()", 0

write_sob_string:
	push rbp
	mov rbp, rsp

	push rsi

	mov rax, 0
	mov rdi, .double_quote
	call printf
	
	pop rsi

	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_DOUBLEQUOTE
	je .ch_doublequote
	cmp rbx, CHAR_BACKSLASH
	je .ch_backslash
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx
	jmp .printf

.ch_doublequote:
	mov rdi, .fs_doublequote
	mov rsi, rbx
	jmp .printf

.ch_backslash:
	mov rdi, .fs_backslash
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	pop rbp
	ret
section .data
.double_quote:
	db CHAR_DOUBLEQUOTE, 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0
.fs_doublequote:
	db CHAR_BACKSLASH, CHAR_DOUBLEQUOTE, 0
.fs_backslash:
	db CHAR_BACKSLASH, CHAR_BACKSLASH, 0

write_sob_pair:
	push rbp
	mov rbp, rsp

	push rsi
	
	mov rax, 0
	mov rdi, .open_paren
	call printf

	mov rsi, [rsp]

	CAR rsi, rsi
	call write_sob

	mov rsi, [rsp]
	CDR rsi, rsi
	call write_sob_pair_on_cdr
	
	add rsp, 1*8
	
	mov rdi, .close_paren
	mov rax, 0
	call printf

	pop rbp
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov bl, byte [rsi]
	cmp bl, T_NIL
	je .done
	
	cmp bl, T_PAIR
	je .cdrIsPair
	
	push rsi
	
	mov rax, 0
	mov rdi, .dot
	call printf
	
	pop rsi

	call write_sob
	jmp .done

.cdrIsPair:
	CDR rbx, rsi
	push rbx
	CAR rsi, rsi
	push rsi
	
	mov rax, 0
	mov rdi, .space
	call printf
	
	pop rsi
	call write_sob

	pop rsi
	call write_sob_pair_on_cdr

.done:
	pop rbp
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

write_sob_symbol:
	push rbp
	mov rbp, rsp

	SYMBOL_VAL rsi, rsi
	
	STRING_LENGTH rcx, rsi
	STRING_ELEMENTS rax, rsi

	mov rdx, rcx

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rcx, rdx
	jne .ch_simple
	cmp rbx, '+'
	je .ch_hex
	cmp rbx, '-'
	je .ch_hex
	cmp rbx, 'A'
	jl .ch_hex

.ch_simple:
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	pop rbp
	ret
	
section .data
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	

write_sob_closure:
	push rbp
	mov rbp, rsp

	CLOSURE_CODE rdx, rsi
	CLOSURE_ENV rsi, rsi

	mov rdi, .closure
	mov rax, 0
	call printf

	pop rbp
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

section .text
write_sob:
	mov rbx, 0
	mov bl, byte [rsi]	
	jmp qword [.jmp_table + rbx * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_rational, write_sob_float, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair

section .text
write_sob_if_not_void:
	mov rsi, rax
	mov bl, byte [rsi]
	cmp bl, T_VOID
	je .continue

	call write_sob
	
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0
