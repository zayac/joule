open Core.Std

exception MessageFormat_Error of string

let gen_class_constraints classes left right =
  let constrs = ref [] in
  let open Term in
  let _ =
    match left, right with
    | Choice (left_choice, _), Choice (right_choice, _) ->
      begin
        String.Map.iter2 left_choice right_choice
          ~f:(fun ~key ~data ->
            match data with
            | `Both ((_, (Record (lr, _))), (_, (Record (rr, _)))) ->
              begin
                String.Map.iter2 lr rr
                  ~f:(fun ~key ~data ->
                    match data with
                    | `Both ((_, Var lv), (_, Var rv))
                        when String.Map.mem classes lv && String.Map.mem classes rv ->
                      let r = String.Map.find_exn classes lv in
                      let filtered = String.Map.filter r ~f:(fun ~key ~data -> not (String.contains key '(')) in
                      constrs := !constrs @ [Var rv, Record (filtered, None)]
                    | _ -> ()
                  )
              end
            | _ -> ()
          )
      end
    | _ -> raise (MessageFormat_Error ("wrong format of a term"))
  in
  !constrs
