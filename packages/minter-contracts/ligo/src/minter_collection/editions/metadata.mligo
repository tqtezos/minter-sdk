type contract_metadata_storage = {
  metadata : (string, bytes) big_map 
; dummy_field : unit 
}
end

let metadata (u, store : unit * contract_metadata_storage) 
  : operation list  * contract_metadata_storage = 
    ([] : operation list), store