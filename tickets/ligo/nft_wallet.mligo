type send_parameter =
  [@layout:comb]
  {destination : nat ticket contract;
   ticket_id : nat}

type receive_parameter = (nat ticket)

type metadata = (string, bytes) map

type token_metadata = (nat, (nat * metadata)) big_map

type auction_contract_type =
  [@layout:comb]
  {opening_price : nat;
  set_reserve_price : nat;
  set_start_time : timestamp;
  set_round_time : int;
  ticket : (nat ticket)}

type auction_parameter =
  [@layout:comb]
  {destination : (auction_contract_type contract);
  opening_price : nat;
  reserve_price : nat;
  start_time : timestamp;
  round_time : int;
  ticket_id : nat}

type parameter =
  | Mint of metadata
  | Burn of nat
  | Receive of receive_parameter
  | Send of send_parameter
  | Auction of auction_parameter

type storage =
  [@layout:comb]
  {admin : address;
   tickets : (nat, nat ticket) big_map;
   current_id : nat;
   token_metadata :  token_metadata
   }

let main (arg : parameter * storage) : operation list * storage =
  begin
    assert (Tezos.amount = 0mutez);
    let (p,storage) = arg in
    let {admin = admin ; tickets = tickets; current_id = current_id; token_metadata = token_metadata} = storage in
    ( match p with
      | Receive ticket -> begin
        let ((_,(_, qty)), ticket) = Tezos.read_ticket ticket in
        let (_, tickets) = Big_map.get_and_update current_id (Some ticket) tickets in
        assert (qty = 1n);
        (([] : operation list), {admin = admin; tickets = tickets; current_id = current_id + 1n; token_metadata = token_metadata})
        end
      | Send send -> begin
        assert (Tezos.sender = admin) ;
        let (ticket, tickets) = Big_map.get_and_update send.ticket_id (None : nat ticket option) tickets in
        ( match ticket with
          | None -> (failwith "no tickets" : operation list * storage)
          | Some ticket ->
              let op = Tezos.transaction ticket 0mutez send.destination in
              ([op], {admin = admin; tickets = tickets; current_id = current_id; token_metadata = token_metadata})
        )
      end
      | Mint md -> begin
        assert (Tezos.sender = admin);
        let ticket = Tezos.create_ticket current_id 1n in
        let (_, tickets) = Big_map.get_and_update current_id (Some ticket) tickets in
        let token_metadata = Big_map.update current_id (Some (current_id, md)) token_metadata in
        (([] : operation list), {admin = admin; tickets = tickets; current_id = current_id + 1n; token_metadata = token_metadata})
      end
      | Burn ticket_id -> begin
        assert (Tezos.sender = admin);
        let tickets = Big_map.update ticket_id (None : (nat ticket) option) tickets in
        (([] : operation list), {admin = admin; tickets = tickets; current_id = current_id + 1n; token_metadata = token_metadata})
      end
      | Auction auction -> begin
        assert (Tezos.sender = admin);
        let (ticket, tickets) = Big_map.get_and_update auction.ticket_id (None : (nat ticket) option) tickets in
        ( match ticket with
          | None -> (failwith "no tickets" : operation list * storage)
          | Some ticket ->
              let auction_params : auction_contract_type = {
                opening_price = auction.opening_price;
                set_reserve_price = auction.reserve_price;
                set_start_time = auction.start_time;
                set_round_time = auction.round_time;
                ticket = ticket
              } in
              let op = Tezos.transaction auction_params 0mutez auction.destination in
              ([op], {admin = admin; tickets = tickets; current_id = current_id; token_metadata = token_metadata})
        )
      end
    )
  end
