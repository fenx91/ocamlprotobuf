open Printf

let main () = 
  let silent = ref false in 
  let args = [
    "--silent", Arg.Set silent, "do not display a prompt or reminder"
  ] in 
  Arg.parse args (fun _ -> ()) "" ; 

  if not !silent then begin 
    flush stdout ; 
  end ; 
  let lexbuf = Lexing.from_channel stdin in
  let pf_command = Parse.com Lex.initial lexbuf in
  let env = Analyzer.empty_env in
  if not !silent then begin 
  let env_n = Analyzer.check_com pf_command env in
  ignore(env_n);  
printf "\n#include <iostream>\n#include <fstream>\n#include <string>\n";
    print_string (Pf.find_include pf_command) ;
    printf "using namespace std;\n\nint main() {\n";
    print_string (Pf.com_to_str pf_command) ; 
    printf ";\nreturn 0;\n}"
  end ;  
exit 0 
;;
main () ;;
