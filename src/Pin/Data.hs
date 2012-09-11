module Pin.Data where


data PinRequest =
  PinRequest {
      pinAmount :: Int -- FIX Data.Fixed
    , pinDescription :: String
    , pinEmail :: String
    , pinIp :: String
    , pinNumber :: String
    , pinExpiryMonth :: Int
    , pinExpiryYear :: Int
    , pinCvc :: Int
    , pinName :: String
    , pinAddress1 :: String
    , pinAddress2 :: String
    , pinCity :: String
    , pinPostcode :: String
    , pinState :: String
    , pinCountry :: String
    }

{--
Request

curl https://api.pin.net.au/1/charges \
-u your-api-key: \
-d "amount=400" \
-d "description=test charge" \
-d "email=roland@pin.net.au" \
-d "ip_address=203.192.1.172" \
-d "card[number]=5123456789012346" \
-d "card[expiry_month]=05" \
-d "card[expiry_year]=2013" \
-d "card[cvc]=519" \
-d "card[name]=Roland Robot" \
-d "card[address_line1]=42 Sevenoaks St" \
-d "card[address_city]=Lathlain" \
-d "card[address_postcode]=6454" \
-d "card[address_state]=WA" \
-d "card[address_country]=AU"

Response

{
  "response":{
    "token":"ch__Eqppblg_DAbmdBYxUigfg",
    "success":true,
    "amount":400,
    "description":"test charge",
    "email":"roland@pin.net.au",
    "ip_address":"203.192.1.172",
    "created_at":"2012-05-23T06:07:42Z",
    "card":{
      "token":"tok_vtcb7jewpBGvIQQvJohPTg",
      "display_number":"XXXX-XXXX-XXXX-2346",
      "scheme":"master",
      "address_line1":"42 Sevenoaks St",
      "address_line2":"",
      "address_city":"Lathlain",
      "address_postcode":"6454",
      "address_state":"WA",
      "address_country":"AU"
    }
  }
}

--}





