let ev d _s = _s + d

(* infer upper bound on return value  of 400+40400*length(data) *)
let rec help data:uint[] i:uint totalSupply:uint _s0 : int =
  if i<data.length then 
      let _s1 = ev 1 _s0 in (***) let a = address(  data[i] & D160-1 ) in
      let _s2 = ev 1 _s1 in (***) let amount = data[i] / D160 in
      if (balanceOf[a] == 0) then begin
        let _s3 = ev 1 _s2 in (***) balanceOf[a] = amount;
        let _s4 = ev 1 _s3 in (***) help data (i+1) (totalSupply + amount) _s4; 
      end else 
        let _s3 = ev 1 _s2 in (***)  help data (i+1) totalSupply _s3
  else
    _s0 (***)  

let fill(uint[] data) =
  let _s1 = 0 in
  if ((msg.sender != owner)||(Sealed)) 
    begin ev 1 _s; throw; end
  else begin ev 1 _s; help data 0 _s1 end 
