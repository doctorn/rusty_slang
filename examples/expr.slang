(* (fun (x: int) -> x end) 3 *)

let fib(m : int) : int = 
  if m = 0 then 
    0
  else 
    if m = 1 then 
      1
    else 
      fib (m - 1) + fib (m - 2) 
    end
  end 
in
  fib 10
end

(*
let x : int ref = ref 3 in
  begin
    x := 4;
    !x
  end
end
*)

let fib (m : int) : int =
  let i : int ref = ref 0 
  in let a : int ref = ref 0 
  in let b : int ref = ref 1 
  in let c : int ref = ref (!a + !b) in
    begin
      while !i < m do
        begin
          i := !i + 1;
          c := !a + !b;
          a := !b;
          b := !c
        end
      end;
      !a
    end
  end
  end
  end
  end
in
  fib 30
end

(*
let add : int -> int -> int = 
  fun (x : int) -> 
    fun (y : int) -> x + y end
  end
in
  add 3 4
end
*)

(* (fun (x : int ref) -> 
  begin 
    while !x < 10 do
      x := !x + 1
    end;
    !x
  end
end) ref 3 *)

(* let x : int ref = ref 0 in
  begin
    while !x < 10 do
      x := !x + 1
    end;
    !x
  end
end *)

(*
let i : int ref = ref 0 in
  while !i < 1000 do
    begin
      let x : int = 3 in
        let y : int = 4 in
          let z : int = 5 in
            let w : int = 6 in
              let a : int = 1 in
                let b : int = 1 in
                  let c : int = 1 in
                    let d : int = 1 in
                      x + y + z + w + a + b + c + d
                    end
                  end
                end
              end
            end
          end
        end
      end;
      i := !i + 1
    end
  end
end
*)

(* 3 * ((8 + 17) * (2 - 6)) *)
