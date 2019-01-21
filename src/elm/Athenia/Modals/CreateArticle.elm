module Athenia.Modals.CreateArticle exposing (..)

import Athenia.Api as Api exposing (Token)
import Athenia.Components.LoadingIndicator as LoadingIndicator
import Athenia.Models.User.User as User
import Athenia.Models.Wiki.Article as Article
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


-- MODEL


type alias Model =
    { article : Article.CreateModel
    , token : Token
    , visibility : Modal.Visibility
    , showLoading : Bool
    }


init : User.Model -> Token -> Model
init user token =
    { article = Article.initCreateModel user
    , token = token
    , visibility = Modal.hidden
    , showLoading = False
    }


-- Create the view for the article creation modal
view : Model -> Html Msg
view model =
    div []
        [ Modal.config Cancel
            |> Modal.large
            |> Modal.h4 [] [ text "Create Article" ]
            |> Modal.body []
                [ Html.form [ onSubmit Confirm ]
                    [ Grid.containerFluid []
                        [ Grid.row []
                            [ Grid.col
                                []
                                [ Input.text
                                    [ Input.large
                                    , Input.placeholder "Enter Article Title"
                                    , Input.onInput EnteredTitle
                                    , Input.value model.article.title
                                    , Input.attrs [required True]
                                    ]
                                ]
                            ]
                        , Grid.row [ Row.attrs [class "button-wrapper"] ]
                            [ Grid.col
                                [ Col.xs6 ]
                                [ Button.button
                                    [ Button.info
                                    , Button.disabled model.showLoading
                                    , Button.onClick Cancel
                                    ]
                                    [ text "Cancel" ]
                                ]
                            , Grid.col
                                [ Col.xs6 ]
                                [ Html.input
                                    [ class "btn btn-primary"
                                    , disabled model.showLoading
                                    , type_ "submit"
                                    ]
                                    [ ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> Modal.view model.visibility
        , LoadingIndicator.view model.showLoading
        ]



-- UPDATE


type Msg
    = EnteredTitle String
    | Cancel
    | Confirm
    | CompletedArticleCreate (Result Http.Error Article.Model)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EnteredTitle title ->
            let
                article = model.article
            in
                ( { model
                    | article =
                        { article
                            | title = title
                        }
                }
                , Cmd.none
                )

        Cancel ->
            ( hide model
            , Cmd.none
            )

        Confirm ->
            ( { model
                | showLoading = True
            }
            , createArticle model.token model.article
            )

        CompletedArticleCreate _ ->
            (model, Cmd.none)


hide : Model -> Model
hide model =
    { model
        | visibility = Modal.hidden
    }


show : Model -> Model
show model =
    { model
        | visibility = Modal.shown
    }


-- HTTP


createArticle : Token -> Article.CreateModel -> Cmd Msg
createArticle token article =
    Http.send CompletedArticleCreate
        <| Api.createArticle token article

