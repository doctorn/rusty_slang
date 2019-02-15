let x : int = 3 in
  let y : unit -> int = fun (y : unit) -> x end in
    y ()
  end
end
