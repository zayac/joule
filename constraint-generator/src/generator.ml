open Core.Std

exception MessageFormat_Error of string

module Node = struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end

module Edge = struct
  type t = int * int
  let compare = compare
  let default = 0, 0
end
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(*let netlist = G.empty*)
let in_term_map = ref String.Map.empty
let out_term_map = ref String.Map.empty

let match_in_channel s =
  let l = String.split ~on:'-' s in
  match l with
  | [] -> None, s
  | hd :: tl ->
    Some (int_of_string hd), String.concat ~sep:"-" tl

let match_out_channel s =
  let l = String.split ~on:'-' s in
  match l with
  | [] -> None, s
  | _ ->
    let first, last = List.split_n l (List.length l - 1) in
    Some (int_of_string (List.hd_exn last)), String.concat ~sep:"-" first

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
                    | `Both ((_, DownVar lv), (_, DownVar rv))
                        when String.Map.mem classes lv && String.Map.mem classes rv ->
                      let r = String.Map.find_exn classes lv in
                      let filtered = String.Map.filter r ~f:(fun ~key ~data -> not (String.contains key '(')) in
                      constrs := !constrs @ [DownVar rv, Record (filtered, None)]
                    | _ -> ()
                  )
              end
            | _ -> ()
          )
      end
    | _ -> raise (MessageFormat_Error ("wrong format of a term"))
  in
  !constrs
