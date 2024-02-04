type token =
  Tok_incr_case
  | Tok_decr_case
  | Tok_incr_ptr
  | Tok_decr_ptr
  | Tok_begin_loop
  | Tok_end_loop
  | Tok_put_char
  | Tok_get_char

type ast =
  AST_loop of ast list
  | AST_instr of token

let print = Printf.printf "%s\n"

let lex input =
  let output = ref [] in
  for i = 0 to String.length input - 1 do
    match input.[i] with
    | '+' -> output := Tok_incr_case :: !output
    | '-' -> output := Tok_decr_case :: !output
    | '>' -> output := Tok_incr_ptr :: !output
    | '<' -> output := Tok_decr_ptr :: !output
    | '[' -> output := Tok_begin_loop :: !output
    | ']' -> output := Tok_end_loop :: !output
    | '.' -> output := Tok_put_char :: !output
    | ',' -> output := Tok_get_char :: !output
    | _ -> ();
  done;
  List.rev !output

let rec parse (toks : token list) =
  match toks with
  | [] -> ([], [])
  | Tok_begin_loop :: q ->
    let cont, q = parse q in
    let l, q = parse q in
    (AST_loop(cont) :: l, q)
  | Tok_end_loop :: q ->
    ([], q)
  | t :: q ->
    let l, q = parse q in
    (AST_instr(t) :: l, q)

(*
 * bol_fmt: Text for the beginning of loops
 * eol_fmt: Text for the end of loops
 * header_fmt: The first instructions to be loaded
 *)
let compile (tree : ast list) compile_instr header_fmt bol_fmt eol_fmt footer_fmt =
  let rec aux tree lbl_id =
    match tree with
    | [] -> lbl_id
    | AST_instr (tok) :: q -> (
      compile_instr tok;
      aux q lbl_id
    )
    | AST_loop (ast) :: q -> (
      Printf.printf "l_%d:\n" lbl_id;
      Printf.printf bol_fmt lbl_id;
      let n_lbl_id = aux ast (lbl_id+1) in
      Printf.printf eol_fmt lbl_id;
      Printf.printf "l_%d_n:\n" lbl_id;
      aux q n_lbl_id
    )
    in
  print header_fmt;
  ignore (aux tree 0);
  print footer_fmt

exception ParsingException


let z80_compile_instr tok =
  match tok with
  | Tok_incr_case -> (
    print "ld a, (hl)";
    print "inc a";
    print "ld (hl), a"
  )
  | Tok_decr_case -> (
    print "ld a, (hl)";
    print "dec a";
    print "ld (hl), a"
  )
  | Tok_incr_ptr -> print "inc hl"
  | Tok_decr_ptr -> print "dec hl"
  | Tok_put_char -> print "put_char"
  | Tok_get_char -> print "get_char"
  | _ -> failwith "Impossible"

let z80_compile _ input =
  compile
    input z80_compile_instr
    "ld hl, buffer\nld b, 0"
    "ld a, (hl)\nand a\njp  z, l_%d_n\n"
    "ld a, (hl)\nand a\njp nz, l_%d\n"
    ""

let x86_compile_instr tok =
  match tok with
  | Tok_incr_case -> print "incb (%eax)"
  | Tok_decr_case -> print "decb (%eax)"
  | Tok_incr_ptr -> print "incl %eax"
  | Tok_decr_ptr -> print "decl %eax"
  | Tok_put_char -> print "put_char"
  | Tok_get_char -> print "get_char"
  | _ -> failwith "Impossible"
      
let x86_compile buf_size input =
  compile
    input x86_compile_instr
    "movl buffer, %eax"
    "cmpb (%%eax), %%bl\nand %%bl, %%bl\njz l_%d_n\n"
    "movb (%%eax), %%bl\nand %%bl, %%bl\njnz l_%d\n"
    (
      ".data\n" ^
      "buffer:\n" ^
      "times " ^ (string_of_int buf_size) ^ " db 0\n"
    )

let mips_compile_instr tok =
  match tok with
  | Tok_incr_case -> print (
      "lw $a0, 0($a1)\n" ^
      "addi $a0, $a0, 1\n" ^
      "sw $a0, 0($a1)"
    )
  | Tok_decr_case -> print (
      "lw $a0, 0($a1)\n" ^
      "addi $a0, $a0, -1\n" ^
      "sw $a0, 0($a1)"
    )
  | Tok_incr_ptr -> print "addi $a1, $a1, 4"
  | Tok_decr_ptr -> print "addi $a1, $a1, -4"
  | Tok_put_char -> print (
      "ori $a0, $a0, 11\n" ^
      "lw $a0, 0($a1)\n" ^
      "syscall"
    )
  | Tok_get_char -> print (
      "ori $a0, $a0, 12\n" ^
      "syscall\n" ^
      "sw $v0, 0($a1)"
    )
  | _ -> failwith "Impossible"
      
let mips_compile buf_size input =
  compile
    input mips_compile_instr
    ".text\nla $a1, buffer"
    "lw $a0, 0($a1)\nbneq $a0, $zero, 1\nj l_%d_n\n"
    "lw $a0, 0($a1)\nbeq  $a0, $zero, 1\nj l_%d_n\n"
    (
      "ori $v0, $v0, 10\n" ^
      "syscall\n" ^
      ".data\n" ^
      "buffer:\n" ^
      ".space " ^ (string_of_int (4*buf_size)) ^ "\n"
    )
    

let read_lines chan =
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
  List.rev !lines

let () =
  assert (Array.length Sys.argv <= 3);
  let parse_with_exception input = 
    let tree, remaining = parse (lex input) in
    if remaining <> []
    then (raise ParsingException)
    else tree
  in
  if Array.length Sys.argv < 3
  then print "bf_compiler [architecture] [buffer size]"
  else
    let buf_size = int_of_string (Sys.argv.(2)) in
    match String.lowercase_ascii (Sys.argv.(1)) with
    | "z80" -> List.iter
      (fun s -> z80_compile buf_size (parse_with_exception s))
      (read_lines Stdlib.stdin)
    | "x86" -> List.iter
      (fun s -> x86_compile buf_size (parse_with_exception s))
      (read_lines Stdlib.stdin)
    | "mips" -> List.iter
      (fun s -> mips_compile buf_size (parse_with_exception s))
      (read_lines Stdlib.stdin)
    | _ -> failwith "Architecture invalide"
