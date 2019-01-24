let y: int + int = inl int 3 in
  case y 
    of inl (x: int) -> x + 1 
     | inr (x: int) -> x + 2
  end
end
