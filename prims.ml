(* 
   This module contains code to generate the low-level implementation of
   parts of the standard library procedures.
   The module works by defining a hierarchy of templates, which call each other
   to form complete routines. See the inline comments below for more information
   on the templates and the individual routines.

   Note that the implementation below contain no error handling or correctness-checking
   of any kind. This is because we will not test your compilers on invalid input.
   However, adding correctness-checking and error handling *as general templates* would be
   rather simple.
 *)
module type PRIMS = sig
  val procs : string;;
end

module Prims : PRIMS = struct
  (* This is the most basic routine template. It takes a body and label name, and
     creates the standard x86-64 bit routine form.
     All other templates and routine-generation functions depend on this template. *)
  let make_routine label body =
    label ^ ":
       push rbp
       mov rbp, rsp 
       " ^ body ^ "
         pop rbp
         ret";;

  (* Many of the low-level stdlib procedures are predicate procedures, which perform 
     some kind of comparison, and then return one of the constants sob_true or sob_false.
     Since this pattern repeats so often, we have a template that takes a body, and a type
     of condition to test for jump, and generates an assembly snippet that evaluated the body,
     and return true or false, depending on the type of condition. *)
  let return_boolean jcc body =
    body ^ "
      " ^ jcc ^ " .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:";;

  (* 
     Many of the predicates just test some kind of equality (or, equivalently, if the 
     zero flag is set), so this is an auxiliary function dedicated to equality-testing predicates. 
     Note how we make use of currying here.
   *)
  let return_boolean_eq = return_boolean "je";;
  
  (* 
     Almost all of the stdlib function take 1 or more arguments. Since all of the variadic procedures
     are implemented in the high-level scheme library code (found in stdlib.scm), we only have to deal
     with 1,2 or 3 arguments.
     These helper functions inject instructions to get parameter values off the stack and into registers
     to work with.
     The argument register assignment follows the x86 64bit Unix ABI, because there needs to be *some*
     kind of consistency, so why not just use the standard ABI.
     See page 22 in https://raw.githubusercontent.com/wiki/hjl-tools/x86-psABI/x86-64-psABI-1.0.pdf
   *)
  let make_unary label body = make_routine label ("mov rsi, PVAR(0)\n\t" ^ body);;
  let make_binary label body = make_unary label ("mov rdi, PVAR(1)\n\t" ^ body);;
  let make_tertiary label body = make_binary label ("mov rdx, PVAR(2)\n\t" ^ body);;

  (* All of the type queries in scheme (e.g., null?, pair?, char?, etc.) are equality predicates
     that are implemented by comparing the first byte pointed to by PVAR(0) to the relevant type tag.
     so the only unique bits of each of these predicates are the name of the routine (i.e., the label), 
     and the type tag we expect to find.
     The implementation of the type-queries generator is slightly more complex, since a template and a label
     name aren't enough: we need to generate a routine for every (label * type_tag) pair (e.g., the routine 
     `is_boolean` should test for the T_BOOL type tag).
     We have a list of pairs, associating each predicate label with the correct type tag, and map the templating 
     function over this list. Note that the query template function makes use of some of the other templating 
     functions defined above: `make_unary` (predicates take only one argument) and `return_boolean_eq` (since 
     these are equality-testing predicates).
   *)
  let type_queries =
    let queries_to_types = [
        "boolean", "T_BOOL"; "flonum", "T_FLOAT"; "rational", "T_RATIONAL"; "pair", "T_PAIR";
        "null", "T_NIL"; "char", "T_CHAR"; "string", "T_STRING"; "symbol", "T_SYMBOL";
        "procedure", "T_CLOSURE"
      ] in
    let single_query name type_tag =
      make_unary (name ^ "?")
        (return_boolean_eq ("mov sil, byte [rsi]\n\tcmp sil, " ^ type_tag)) in
    String.concat "\n\n" (List.map (fun (a, b) -> single_query a b) queries_to_types);;

  (* The arithmetic operation implementation is multi-tiered:
     - The low-level implementations of all operations are binary, e.g. (+ 1 2 3) and (+ 1) are not 
       supported in the low-level implementation.
     - The low-level implementations only support same-type operations, e.g. (+ 1 2.5) is not supported
       in the low-level implementation. This means each operation has two low-level implementations, one
       for floating-point operands, and one for fractional operands.
     - Each pair of low-level operation implementations is wrapped by a dispatcher which decides which 
       of the two implementations to call (by probing the types of the operands).
     - The high-level implementations (see stdlib.scm) make use of a high-level dispatcher, that is in charge
       of performing type conversions as necessary to satisfy the pre-conditions of the low-level implementations.
     
     Operations on floating-point operands:
     -------------------------------------
     The implementations of binary floating point arithmetic operations contain almost identical code. The
     differences are the name (label) of the routines, and the arithmetic instruction applied to 
     the two arguments. Other than that, they are all the same: binary routines which load the values
     pointed at by PVAR(0) and PVAR(1) into SSE2 registers, compute the operation, create a new sob_float
     on the heap with the result, and store the address of the sob_float in rax as the return value.
     This allows us to easily abstract this code into a template that requires a label name and its matching
     arithmetic instruction (which are paired up in the op_map).

     Operations on fractional operands:
     ----------------------------------
     The addition and multiplication operations on rational numbers are similar to each other: both load 2 arguments,
     both deconstruct the arguments into numerator and denominator, both allocate a sob_rational to store the result
     on the heap, and both move the address of this sob_rational into rax as the return value. The only differences 
     are the routine name (label), and the implementation of the arithmetic operation itself.
     This allows us to easily abstract this code into a template that requires a label name and its matching
     arithmetic instruction (which are paired up in the op_map).
     
     Unlike in the case of floating point arithmetic, rational division is treated differently, and is implemented by
     using the identity (a/b) / (c/d) == (a/b) * (d/c).
     This is done by inverting the second arg (in PVAR(1)) and tail-calling fraction multiplication (`jmp mul`).

     Comparators:
     ------------
     While the implementation of the Comparators is slightly more complex, since they make use of `return_boolean`,
     the idea is the same as the arithmetic operators.
     A couple of things to note:
     - `eq.flt` can collapse to a bitwise comparison (like in the case of integers in C), while `eq.rat` involves
       comparing the numerator and denominator separately, due to our fraction representation using 128 bits
       and not 64 bits.
     - `lt.flt` does not handle NaN, +inf and -inf correctly. This allows us to use `return_boolean jl` for both the
       floating-point and the fraction cases. For a fully correct implementation, `lt.flt` should make use of
       the `ucomisd` opcode and `return_boolean jb` instead (see https://www.felixcloutier.com/x86/ucomisd for more information).
   *)
  let numeric_ops =
    let numeric_op name flt_body rat_body body_wrapper =      
      make_binary name
        (body_wrapper
           ("mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne ." ^ name ^ "_rat
             " ^ flt_body ^ "
             jmp .op_return
          ." ^ name ^ "_rat:
             " ^ rat_body ^ "
          .op_return:")) in      
    let arith_map = [
        "MAKE_RATIONAL(rax, rdx, rdi)
         mov PVAR(1), rax
         pop rbp
         jmp mul", "divsd", "div";
        
        "imul rsi, rdi
	 imul rcx, rdx", "mulsd", "mul";
        
        "imul rsi, rdx
	 imul rdi, rcx
	 add rsi, rdi
	 imul rcx, rdx", "addsd", "add";
      ] in
    let arith name flt_op rat_op =
      numeric_op name
        ("FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  " ^ flt_op ^ " xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)")
        ("DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          " ^ rat_op ^ "
          MAKE_RATIONAL(rax, rsi, rcx)") in
    let comp_map = [
        (* = *)
        return_boolean_eq,
        "NUMERATOR rcx, rsi
	 NUMERATOR rdx, rdi
	 cmp rcx, rdx
	 jne .false
	 DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 cmp rcx, rdx
         .false:",
        "FLOAT_VAL rsi, rsi
	 FLOAT_VAL rdi, rdi
	 cmp rsi, rdi", "eq";

        (* < *)
        return_boolean "jl",
        "DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 NUMERATOR rsi, rsi
	 NUMERATOR rdi, rdi
	 imul rsi, rdx
	 imul rdi, rcx
	 cmp rsi, rdi",
        "FLOAT_VAL rsi, rsi
	 movq xmm0, rsi
	 FLOAT_VAL rdi, rdi
	 movq xmm1, rdi
	 cmpltpd xmm0, xmm1
     movq rsi, xmm0
     cmp rsi, 0", "lt";
      ] in
    let comparator comp_wrapper name flt_body rat_body = numeric_op name flt_body rat_body comp_wrapper in
    (String.concat "\n\n" (List.map (fun (a, b, c) -> arith c b a (fun x -> x)) arith_map)) ^
      "\n\n" ^
        (String.concat "\n\n" (List.map (fun (a, b, c, d) -> comparator a d c b) comp_map));;


  (* The following set of operations contain fewer similarities, to the degree that it doesn't seem that 
     creating more fine-grained templates for them is beneficial. However, since they all make use of
     some of the other templates, it is beneficial to organize them in a structure that enables
     a uniform mapping operation to join them all into the final string.*)
  let misc_ops =
    let misc_parts = [
        (* string ops *)
        "STRING_LENGTH rsi, rsi
         MAKE_RATIONAL(rax, rsi, 1)", make_unary, "string_length";
        
        "STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         mov sil, byte [rsi]
         MAKE_CHAR(rax, sil)", make_binary, "string_ref";
        
        "STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         CHAR_VAL rax, rdx
         mov byte [rsi], al
         mov rax, SOB_VOID_ADDRESS", make_tertiary, "string_set";
        
        "NUMERATOR rsi, rsi
         CHAR_VAL rdi, rdi
         and rdi, 255
         MAKE_STRING rax, rsi, dil", make_binary, "make_string";
        
        "SYMBOL_VAL rsi, rsi
	 STRING_LENGTH rcx, rsi
	 STRING_ELEMENTS rdi, rsi
	 push rcx
	 push rdi
	 mov dil, byte [rdi]
	 MAKE_CHAR(rax, dil)
	 push rax
	 MAKE_RATIONAL(rax, rcx, 1)
	 push rax
	 push 2
	 push SOB_NIL_ADDRESS
	 call make_string
	 add rsp, 4*8
	 STRING_ELEMENTS rsi, rax   
	 pop rdi
	 pop rcx
	 cmp rcx, 0
	 je .end
         .loop:
	 lea r8, [rdi+rcx]
	 lea r9, [rsi+rcx]
	 mov bl, byte [r8]
	 mov byte [r9], bl
	 loop .loop
         .end:", make_unary, "symbol_to_string";

        (* the identity predicate (i.e., address equality) *)
        (return_boolean_eq "cmp rsi, rdi"), make_binary, "eq?";

        (* type conversions *)
        "CHAR_VAL rsi, rsi
	 and rsi, 255
	 MAKE_RATIONAL(rax, rsi, 1)", make_unary, "char_to_integer";

        "NUMERATOR rsi, rsi
	 and rsi, 255
	 MAKE_CHAR(rax, sil)", make_unary, "integer_to_char";

        "DENOMINATOR rdi, rsi
	 NUMERATOR rsi, rsi 
	 cvtsi2sd xmm0, rsi
	 cvtsi2sd xmm1, rdi
	 divsd xmm0, xmm1
	 movq rsi, xmm0
	 MAKE_FLOAT(rax, rsi)", make_unary, "exact_to_inexact";

        "NUMERATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)", make_unary, "numerator";

        "DENOMINATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)", make_unary, "denominator";

        (* GCD *)
        "xor rdx, rdx
	 NUMERATOR rax, rsi
         NUMERATOR rdi, rdi
       .loop:
	 and rdi, rdi
	 jz .end_loop
	 xor rdx, rdx 
	 div rdi
	 mov rax, rdi
	 mov rdi, rdx
	 jmp .loop	
       .end_loop:
	 mov rdx, rax
         MAKE_RATIONAL(rax, rdx, 1)", make_binary, "gcd";  

         (* car *)
         "CAR rax, rsi", make_unary, "car";
         
         (* cdr *)
         "CDR rax, rsi", make_unary, "cdr";

         (* set-car! *)
         "mov qword [rsi+TYPE_SIZE], rdi
   mov rax, SOB_VOID_ADDRESS", make_binary, "set_car";

         (* set-cdr! *)
         "mov qword [rsi+TYPE_SIZE+WORD_SIZE], rdi
   mov rax, SOB_VOID_ADDRESS", make_binary, "set_cdr";

         (* cons *)
         "MAKE_PAIR(rax, rsi, rdi)", make_binary, "cons";
         
      ] in
    String.concat "\n\n" (List.map (fun (a, b, c) -> (b c a)) misc_parts);;

    (* apply (variadic) *)
    let apply_variadic = 
      let begin_apply = 
        "apply:\npush rbp\nmov rbp, rsp\n"
      in
      let get_last_arg_to_rax = 
        "; Getting the last arg (n-1) to rax\n"
      ^ "mov rax, qword [ARGS_COUNT_POSITION]\n" 
      ^ "mov rax, PVAR(rax-1)"
      in 
      let push_all_members_of_list_to_stack = 
        "; We need to prepare a fake frame of the procedure in arg0 before applying the logic\n"
        ^ "; of ApplicTP' frame copy. first, copy all the args in the reverse order\n"
        ^ ";"
        ^ "; assuming the last arg (which is a list) is in RAX, travel the list and push the consts to the stack\n"
        ^ "; This will create an array of constants on the stack, NOT in the correct order\n"
        ^ "; after this, we should reverse this array (before applying the ApplicTP' logic)\n"
        ^ ";"
        ^ "; rsi will store the location of the current rsp (for the future reverse)\n"
        ^ "mov rsi, rsp\n"
        ^ "; rcx will be the counter of the length of the list\n"
        ^ "; at the end, we will add this number to the current args count (minus 1) to get the actual number of args\n"
        ^ "mov rcx, 0\n"
        ^ ".apply_variadic_push_last_list_to_stack_loop:\n"
        ^ "; compare current list to NIL\n" 
        ^ "cmp rax, SOB_NIL_ADDRESS\n"
        ^ "; if equal, we are done\n"
        ^ "je .apply_variadic_push_last_list_to_stack_loop_end\n"
        ^ "; not NIL, push car to stack\n"
        ^ "CAR rbx, rax\n"
        ^ "push rbx\n"
        ^ "CDR rax, rax\n"
        ^ "inc rcx\n"
        ^ "jmp .apply_variadic_push_last_list_to_stack_loop\n"
        ^ ".apply_variadic_push_last_list_to_stack_loop_end:\n"
        ^ "; rdi will save the current location of rsp for the copy\n"
        ^ "mov rdi, rsp\n"
      in
      let reverse_array_between_rsi_and_rdi = (* TODO: Debug carefully! *)
        ".apply_variadic_reverse_loop:\n"
        ^ "; first check if rsi <= rdi, if so - there is nothing to reverse\n"
        ^ "cmp rsi, rdi\n"
        ^ "jbe .apply_variadic_reverse_loop_end\n"
        ^ "; move rsi one qword below\n"
        ^ "sub rsi, 8\n"
        ^ "; replace rsi and rdi values\n"
        ^ "mov rax, qword [rsi]\n"
        ^ "mov rbx, qword [rdi]\n"
        ^ "mov qword [rdi], rax\n"
        ^ "mov qword [rsi], rbx\n"
        ^ "; advance rdi one qword above\n"
        ^ "add rdi, 8\n"
        ^ "jmp .apply_variadic_reverse_loop\n"
        ^ ".apply_variadic_reverse_loop_end:\n"
      in
      let copy_rest_of_args_to_the_stack = 
        "; Now, below the last rbp on the stack, the last arguments are located in the correct reversed order\n"
        ^ "; before applying the ApplicTP' frame copy logic, we need to copy the rest of the arguemnts for the proc\n"
        ^ "; copy n-2 arguments and push them to stack (n-2),(n-3)...,1\n"
        
        ^ "; Getting the (n-2) arg address to rsi\n"
        ^ "mov rax, qword [ARGS_COUNT_POSITION]\n"
        ^ "lea rsi, [rbp+(2+rax)*WORD_SIZE] ; Get the pointer on stack to PVAR(rax-2)\n"

        ^ "; Push the rest of the args\n"
        ^ "lea rdi, [ARGS_COUNT_POSITION+WORD_SIZE] ; We'll use this value to stop the loop. We want to stop BEFORE the first arg - this is the func to apply\n"
        ^ ".apply_variadic_copy_rest_of_stack_loop:\n"
        ^ "cmp rsi, rdi ; We want to stop BEFORE the first arg - this is the func to apply\n"
        ^ "je .apply_variadic_copy_rest_of_stack_end\n"

        ^ "push qword [rsi]\n"
        ^ "sub rsi, WORD_SIZE\n"
        ^ "jmp .apply_variadic_copy_rest_of_stack_loop\n"
        ^ ".apply_variadic_copy_rest_of_stack_end:\n"
      in
      let push_correct_args_count_to_stack = 
        "; We have the length of the list in rcx, so we need to add it to the current args number (currently in rax) minus 2 (the list and the function to apply)\n"
        ^ "add rcx, rax\n"
        ^ "sub rcx, 2\n"
        ^ "push rcx\n"
      in
      let get_proc_to_rax = "mov rax, PVAR(0) ; Get the closure to rax\n" in

      let push_same_env_to_stack = 
        "; No need to extend the env, we just need to copy it\n"
        ^ "push qword [rax + TYPE_SIZE] ; Push closure env\n"
      in
      let push_old_ret_addr = "push qword [rbp + 8] ; Push old return address\n" in
      
      (* Beginning of copied applic_tp logic *)
      let save_rbp = "mov rdi, rbp ; save current frame base pointer" in
      let save_rsp = "mov rsi, rsp ; save current frame top pointer" in
      let restore_old_frame_pointer = "mov rbp, qword [rbp] ; Restore old frame pointer" in
      let overwrite_existing_frame = ";;; Overwriting Existing Frame\n"
      ^ "; set the number of bytes to copy in rcx\n"
      ^ "mov rcx, rdi\n"
      ^ "sub rcx, rsi\n"
      ^ "; increase rsi to point to the top of the current data to run over with\n"
      ^ "mov rsi, rdi\n"
      ^ "; increase rdi to point to the base of this frame\n"
      ^ "add rdi, WORD_SIZE*3 ; pass old rbp, ret addr, env\n"
      ^ "mov r8, qword[rdi] ; extract args number\n"
      ^ "shl r8, 3 ; multiple by 8\n"
      ^ "add rdi, 8 ; pass args count\n"
      ^ "add rdi, r8 ; pass all args\n"
      ^ "; the copy loop\n"
      ^ ".apply_OverWritingLoop:\n"
      ^ "cmp rcx, 0\n"
      ^ "je .apply_OverWritingLoopEnd\n"
      ^ "sub rdi, 8\n"
      ^ "sub rsi, 8\n"
      ^ "mov r8, qword[rsi]\n"
      ^ "mov qword[rdi], r8\n"
      ^ "sub rcx, 8\n"
      ^ "jmp .apply_OverWritingLoop\n"
      ^ ".apply_OverWritingLoopEnd:\n"
      ^ "; fix stack pointer\n"
      ^ "mov rsp, rdi\n" in
      (* End of copied applic_tp logic *)
      let apply_applic_tp_logic = String.concat "\n" [
        save_rbp;
        save_rsp;
        restore_old_frame_pointer;
        overwrite_existing_frame
      ]
      in
      let jmp_to_procedure_code = 
        "; Assuming the closure is in RAX, jmp to the code\n"
        ^ "CLOSURE_CODE rax, rax ; Move closure code to rax\n"
        ^ "jmp rax\n"
      in
      let apply_code = String.concat "\n" [
        begin_apply;
        get_last_arg_to_rax;
        push_all_members_of_list_to_stack;
        reverse_array_between_rsi_and_rdi;
        copy_rest_of_args_to_the_stack;
        push_correct_args_count_to_stack;
        get_proc_to_rax;
        push_same_env_to_stack;
        push_old_ret_addr;
        apply_applic_tp_logic;
        jmp_to_procedure_code;
      ]
      in
      apply_code
    ;;

  (* This is the interface of the module. It constructs a large x86 64-bit string using the routines
     defined above. The main compiler pipline code (in compiler.ml) calls into this module to get the
     string of primitive procedures. *)
  let procs = String.concat "\n\n" [type_queries ; numeric_ops; misc_ops; apply_variadic];;
end;;
