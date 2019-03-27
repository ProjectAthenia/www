port module Ports.ArticleSocket exposing(..)


port connectArticleSocket: (String, Int) -> Cmd msg


port articleUpdated: (String -> msg) -> Sub msg


port sendUpdateMessage: (String, Int) -> Cmd msg
