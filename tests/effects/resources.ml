let rec help data:uint[] i:uint totalSupply:uint =
  if i<data.length then 
  if (i<data.length) begin
     ev 1; let a = address(  data[i] & D160-1 ) in
     ev 1; let amount = data[i] / D160 in
     if (balanceOf[a] == 0) then
        ev 1; balanceOf[a] = amount;
        ev 1; help data (i+1) (totalSupply + amount); 
     else begin
        ev 1; help data (i+1) totalSupply  
     end
    end

let fill(uint[] data) =
  if ((msg.sender != owner)||(Sealed)) 
    begin ev 1; throw; end
  ev 1; help data 0 
