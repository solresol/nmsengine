> module Nmsengine where
>
> data Address = IPv4 (Int,Int,Int,Int) 
>                deriving (Show,Eq,Ord)
> data Source = Local Address | Remote Address Source 
>               deriving (Show,Eq,Ord)
> type Macaddr = [Char]  -- deriving (Show,Eq,Ord)
> data Netmask = Cidr Int | Bitstring (Int,Int,Int,Int)  deriving (Show,Eq,Ord)
>
> data InputEvent =
>    SystemFlushReset
>  | ManualAddOfSystem Address
>  | ArpTable Address Macaddr Address  
>        -- system we polled to get this data, then 3rd party mac addr,  3rd party ip addr
>  | Interface Address Int Address 
>        -- system we polled to get this data,  which interface index,  what address it has
>  | InterfaceName Address Int String
>        -- system we polled to get this data,  which interface index,  what address it has
>  | InterfaceMask Address Int Netmask
>  | PingDown Address
>  | PingUp Address
>  | OperStatusDown Address Int
>  | OperStatusUp Address Int
>      deriving (Show,Eq,Ord)
>
> instance Read Address where
>   readsPrec _ x = [(IPv4 (read a,read b,read c,read d) ,d') |
>                      (a,a') <- lexDigits x,
>                      (b,b') <- lexDigits (drop 1 a'),
>                      (c,c') <- lexDigits (drop 1 b'),
>                      (d,d') <- lexDigits (drop 1 c') ]
>
> data Interface = IPintf { idx :: Int,
>                           name :: String,
>                           addr :: Address,
>                           netmask :: Maybe Netmask }  
>                   | NonIPintf { idx::Int, name::String }
> data Node = Computer Interface |
>             Router [Interface] |
>             Layer2Connector [Interface]
>
>
> type NodeDatabase = [Node]
>
> data OutEvent =
>        IPconflict | BadSubnetMask | NodeDown
>
> sampleData = [ SystemFlushReset ]
>
> think :: NodeDatabase -> [InputEvent] -> (NodeDatabase,[OutEvent])
> think _ (SystemFlushReset:xs) = think [] xs
> think db ((ManualAddOfSystem addr):xs) 
> think db _  =
> manualAdd db


