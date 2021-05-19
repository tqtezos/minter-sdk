#include "fixed_price_sale_market_tez.mligo"

(*VIEWS*)
let rec activeTezSalesHelper (active_sales, sale_id, s : (sale_tez list) * sale_id * storage) 
  : (sale_tez list) = 
  (if sale_id > s.next_sale_id 
  then active_sales
  else ( match (Big_map.find_opt sale_id s.sales) with 
    | Some sale -> activeTezSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeTezSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveTezSales (u , s : unit * storage) : (sale_tez list) = 
  (activeTezSalesHelper (([] : sale_tez list), 0n,  s))
