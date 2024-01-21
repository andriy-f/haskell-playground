module EnterpriseAction where

import Data.Time.Clock (UTCTime)

type EnterpriseId = String

type FailReason = String

type OKResult = String

data OrderItem = OrderItem {quantity :: Int, productId :: EnterpriseId} deriving (Show, Eq)

data Order = Order
  { orderId :: EnterpriseId,
    items :: [OrderItem],
    createdBy :: EnterpriseId
  }
  deriving (Show, Eq)

type UpdateOrderResult = Either FailReason OKResult

validateRequest :: Order -> UpdateOrderResult
validateRequest order =
  if null (items order)
    then Left "Order must have at least one item"
    else Right "Order is valid"

-- checkPermissions ::

updateOrder :: Order -> UpdateOrderResult
updateOrder order = Right "Order updated"
