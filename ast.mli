type operator = 
Add 
| Sub 
| Mul 
| Div 
| Assn 
| LThan 
| GThan 
| Equals 
| LThanEq 
| GThanEq 
| NotEquals 
| And 
| Or 

tpye expr = 
Binop of expr * operator * expr
| Lit of int