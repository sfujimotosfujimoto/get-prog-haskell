module Ch4 where

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey


genHostRequestBuilder host resource = (\apiKey id -> getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com" "book"

genApiRequestBuilder hostBuilder apiKey = (\id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337haskell"

