let ceil_div (tz_qty, nat_qty : tez * nat) : tez = 
  let ediv1 : (tez * tez) option = ediv tz_qty nat_qty in 
  match ediv1 with 
    | None -> (failwith "DIVISION_BY_ZERO"  : tez) 
    | Some e -> 
       let (quotient, remainder) = e in
       if remainder > 0mutez then (quotient + 1mutez) else quotient