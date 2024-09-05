module MyWebRequest
  ( MyResult,
  )
where

-- TODO consider deleting Enterprise action
type Fail = String

type Success = String

type MyResult s f = Either f s

type UserID = String

type AllowedUsers = [String]

data CreatePostRequest = CreateRecordRequest
  { cookie :: String,
    postTitle :: String,
    postText :: String
  }

parseCookie :: CreatePostRequest -> MyResult (CreatePostRequest, UserID) String
parseCookie req = Right (req, cookie req)

parseCookieFail :: CreatePostRequest -> MyResult (CreatePostRequest, UserID) String
parseCookieFail req = Left "Invalid cookie"

verifyAuth :: AllowedUsers -> (CreatePostRequest, UserID) -> MyResult CreatePostRequest String
verifyAuth allowedUsers (req, userId) = Right req

verifyAuthFail :: (CreatePostRequest, UserID) -> AllowedUsers -> MyResult CreatePostRequest String
verifyAuthFail (req, _) _ = Left "User not allowed"

createPostInDB :: CreatePostRequest -> MyResult String String
createPostInDB req =
  let pTitle = postTitle req
      pText = postText req
   in Right "Done"

createPost :: CreatePostRequest -> MyResult String String
createPost req =
  let verifyAuthAdmins = verifyAuth ["1"]
      join1 x = parseCookie x >>= verifyAuthAdmins >>= createPostInDB
   in join1 req
