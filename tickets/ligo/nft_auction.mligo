type storage_data =
  [@layout:comb]
  {admin : address;
   current_price : nat;
   reserve_price : nat;
   in_progress : bool;
   start_time : timestamp;
   round_time : int}

type storage =
  [@layout:comb]
  {data : storage_data;
   tickets : (nat, nat ticket) big_map}

type configure_parameter =
  [@layout:comb]
  {opening_price : nat;
  set_reserve_price : nat;
  set_start_time : timestamp;
  set_round_time : int;
  ticket : (nat ticket)}

type parameter =
  | Configure of configure_parameter
  | Start of unit
  | Drop_price of nat
  | Buy of (nat ticket contract)
  | Cancel of (nat ticket contract)

let main (arg : parameter * storage) : operation list * storage =
  begin
    let (p,storage) = arg in
    let {data = data; tickets = tickets} = storage in
    ( match p with
        | Configure configure -> begin
            assert (Tezos.source = data.admin);
            assert (not data.in_progress);
            let {opening_price = opening_price; set_reserve_price = set_reserve_price; set_start_time = set_start_time; set_round_time = set_round_time; ticket = ticket} = configure in
            let tickets = Big_map.update 0n (Some ticket) tickets in
            (([] : operation list), {data = {admin = data.admin; current_price = opening_price;
              reserve_price = set_reserve_price; in_progress = false; start_time = set_start_time;
              round_time = set_round_time}; tickets = tickets})
          end
        | Start -> begin
            let now = Tezos.now in
            assert (Tezos.source = data.admin);
            assert (not data.in_progress);
            assert (now >= data.start_time);
            let (t, tickets) = Big_map.get_and_update 0n (None : nat ticket option) tickets in
            ( match t with
               | None -> (failwith "no ticket" : operation list * storage)
               | Some t -> let tickets = Big_map.update 0n (Some t) tickets in
                 (([] : operation list), {data = {data with in_progress = true; start_time = now}; tickets = tickets})
            )
          end
        | Drop_price new_price -> begin
            let now = Tezos.now in
            assert (Tezos.sender = data.admin);
            assert (data.in_progress);
            assert (new_price < data.current_price);
            assert (new_price >= data.reserve_price);
            assert (now > data.start_time +  data.round_time);
            (([] : operation list), {tickets = tickets; data = {data with current_price = new_price; start_time = now}})
          end
        | Buy send_to -> begin
            let now = Tezos.now in
            let purchase_price = Tezos.amount in
            assert (Tezos.sender <> data.admin);
            assert (data.in_progress);
            assert (purchase_price = (data.current_price * 1mutez));
            assert (now <= data.start_time +  data.round_time);
            ( match ((Tezos.get_contract_opt data.admin) : unit contract option) with
                | None -> (failwith "contract does not match" : operation list * storage)
                | Some c -> let op1 = Tezos.transaction () purchase_price c in
                    let (t, tickets) = Big_map.get_and_update 0n (None : nat ticket option) tickets in
                    ( match t with
                        | None -> (failwith "ticket does not exist" : operation list * storage)
                        | Some t -> let op2 = Tezos.transaction t 0mutez send_to in
                           ([op1; op2], {tickets = tickets; data = {data with in_progress = false}})
                    )
            )
          end
        | Cancel return_destination -> begin
            assert (Tezos.sender = data.admin);
            assert (data.in_progress);
            let (t, tickets) = Big_map.get_and_update 0n (None : nat ticket option) tickets in
            ( match t with
                | None -> (failwith "ticket does not exist" : operation list * storage)
                | Some t -> let op = Tezos.transaction t 0mutez return_destination in
                    ([op], {data = {data with in_progress = false}; tickets = tickets})
            )
          end
    )
  end
