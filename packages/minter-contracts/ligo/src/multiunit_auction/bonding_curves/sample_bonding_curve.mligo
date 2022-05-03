(*Means that if at least 3 bids win, the minimum bid value must be 1.*)
let one_third_x (x : nat) : tez =
  0.333333tez * x

let one_third_x_integral (x : nat) : tez = 
  0.166666tez * (x * x)