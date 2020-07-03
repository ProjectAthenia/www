module Components.CRUD.ModelList exposing (..)

import Api exposing (Token)
import Api.Endpoint exposing (Endpoint)
import Models.Page as Page
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Browser.Navigation as Navigation
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.LoadingIndicator as LoadingIndicator
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as ListExtra
import Modals.Confirmation as ConfirmationModal
import Models.Page as Page
import Models.Status as Status
import Url.Builder as UrlBuilder exposing (QueryParameter)
import Utilities.Expands as Expands
import Utilities.ModelHelpers as ModelHelpers exposing (GenericModel)
import Utilities.Order as Order
import Utilities.SearchField as SearchField


type alias DataPage dataModel =
    Page.Model (GenericModel dataModel)


type alias DeleteResult =
    Result Api.Error Status.Model


type alias QueryResult dataModel =
    Result Api.Error (DataPage dataModel)


type Msg dataModel
    = CreateNewRecord
    | OpenDeleteModal Endpoint
    | UpdateRecord Int
    | LoadPage Int
    | SetSearchFieldValue SearchField.Model String
    | ConfirmDeletion Endpoint
    | CancelDeletion
    | NewDataResponse (QueryResult dataModel)
    | DeleteSuccess DeleteResult


type alias Column dataModel =
    { header: String
    , valueCallback: GenericModel dataModel -> Html (Msg dataModel)
    , searchField: SearchField.Model
    }


type alias Configuration dataModel =
    { orders: List Order.Order
    , columns: List (Column dataModel)
    , createDisabled: Bool
    , deleteEnabled: Bool
    }


type alias Model dataModel =
    { sharedConfiguration: SharedConfiguration.Configuration dataModel
    , configuration: Configuration dataModel
    , navigationKey: Navigation.Key
    , loading: Bool
    , columns: List (Column dataModel)
    , models: List (Int, GenericModel dataModel)
    , page: Maybe (DataPage dataModel)
    , deleteModal: Maybe (ConfirmationModal.Model (Msg dataModel))
    , lastLoadedPage: Int
    }


-- All configuration starts here

configure: List Order.Order -> List (Column dataModel) -> Configuration dataModel
configure orders columns =
    { orders = orders
    , columns = columns
    , createDisabled = False
    , deleteEnabled = False
    }


disableCreate : Configuration dataModel -> Configuration dataModel
disableCreate model =
    { model
        | createDisabled = True
    }


enableDelete : Configuration dataModel -> Configuration dataModel
enableDelete model =
    { model
        | deleteEnabled = True
    }


-- All state manipulations happens here

column : String -> (GenericModel dataModel -> Html (Msg dataModel)) -> String -> SearchField.SearchFieldType -> Column dataModel
column header callback searchField searchFieldType =
    { header = header
    , valueCallback = callback
    , searchField = SearchField.model searchField searchFieldType
    }


initialState : SharedConfiguration.Configuration dataModel -> Configuration dataModel -> Navigation.Key -> Token -> (Model dataModel, Cmd (Msg dataModel))
initialState sharedConfiguration configuration navigationKey token =
    let
        model =
            { sharedConfiguration = sharedConfiguration
            , configuration = configuration
            , navigationKey = navigationKey
            , loading = True
            , columns = configuration.columns
            , models = []
            , page = Nothing
            , deleteModal = Nothing
            , lastLoadedPage = 1
            }
    in
    ( model
    , runQuery token model 1
    )


update : Token -> Msg dataModel -> Model dataModel -> (Model dataModel, Cmd (Msg dataModel))
update token msg model =
    case msg of
        SetSearchFieldValue searchField value ->
            let
                updatedModel =
                    { model | columns = updateSearchField searchField value model.columns }
            in
                ( { updatedModel
                    | loading = True
                }
                , runQuery token updatedModel 1
                )

        CreateNewRecord ->
            ( model
            , Navigation.pushUrl model.navigationKey ("/admin/" ++ model.sharedConfiguration.pageUrl ++  "/create")
            )

        OpenDeleteModal endpoint ->
            ( { model
                | deleteModal = Just
                    <| ConfirmationModal.showModal
                    <| ConfirmationModal.initialState ("Are you sure you want to delete this " ++ model.sharedConfiguration.resourceName ++ "?") "This could be a real pain in the ass to undo." (ConfirmDeletion endpoint) CancelDeletion
            }
            , Cmd.none
            )

        CancelDeletion ->
            ( { model
                | deleteModal = Nothing
            }
            , Cmd.none
            )

        ConfirmDeletion endpoint ->
            ( { model
                | deleteModal = Nothing
                , loading = True
            }
            , Api.deleteModel endpoint token DeleteSuccess
            )

        UpdateRecord id ->
            ( model
            , Navigation.pushUrl model.navigationKey ("/admin/" ++ model.sharedConfiguration.pageUrl ++  "/" ++ (String.fromInt id))
            )

        LoadPage pageNumber ->
            ( { model
                | loading = True
            }
            , runQuery token model pageNumber
            )

        NewDataResponse (Err _) ->
            -- TODO handle error properly
            ( model, Cmd.none )

        NewDataResponse (Ok dataPage) ->
            ( { model
                | page = Just dataPage
                , models = ModelHelpers.toKeyValuePairs dataPage.data
                , lastLoadedPage = dataPage.current_page
                , loading = False
            }
            , Cmd.none
            )

        DeleteSuccess (Err _) ->
            -- TODO handle error properly
            ( model, Cmd.none )

        DeleteSuccess (Ok _) ->
            ( { model
              | deleteModal = Nothing
              , loading = False
            }
            , case model.page of
                Just currentPage ->
                    runQuery token model currentPage.current_page
                Nothing ->
                    runQuery token model 1
            )


updateSearchField : SearchField.Model -> String -> List (Column dataModel) -> List (Column dataModel)
updateSearchField searchField value columns =
    ListExtra.updateIf
        (\i -> i.searchField.name == searchField.name)
        (\i -> { i | searchField = SearchField.setValue searchField value } )
        columns

-- All view stuff starts here


view : Model dataModel -> Html (Msg dataModel)
view model =
    div []
        <| List.concat
            [
                [ h2 []
                    [ text ("Mange " ++ model.sharedConfiguration.resourceName ++ "s")
                    , if model.configuration.createDisabled == False then
                        Button.button
                            [ Button.primary
                            , Button.onClick CreateNewRecord
                            , Button.attrs [ class "create_btn" ]
                            ]
                            [ text ("Add " ++ model.sharedConfiguration.resourceName) ]
                    else
                        text ""
                    ]
                , Table.table
                    { options = [ Table.bordered, Table.striped ]
                    , thead = Table.thead []
                        [ Table.tr [] (builderHeader model)
                        ]
                    , tbody = Table.tbody []
                        <| List.map (buildRow model) model.models
                    }
                , div [ class "pagination" ] (buildPagination model.page)
                ]
                , case model.deleteModal of
                    Just modal ->
                        [ ConfirmationModal.view modal
                        ]
                    Nothing ->
                        []
                , [ LoadingIndicator.view model.loading ]
            ]


builderHeader : Model dataModel -> List (Table.Cell (Msg dataModel))
builderHeader model =
    List.concat
        [ [ Table.th [] [ text "#" ] ]
        , List.map builderHeaderCell model.configuration.columns
        , [ Table.th [] [] ]
        , if model.configuration.deleteEnabled then [Table.th [] []] else []
        ]


builderHeaderCell : Column a -> Table.Cell (Msg dataModel)
builderHeaderCell instance =
    Table.th []
        [ p [] [ text instance.header ]
        , case instance.searchField.type_ of
             SearchField.Number ->
                Input.number
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.Equals ->
                Input.text
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.Text ->
                Input.text
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.None ->
                  text ""

             SearchField.Select options ->
                Select.select
                    [ Select.onChange (SetSearchFieldValue instance.searchField)
                    ]
                    (List.map (\(name, label) -> Select.item [ Attributes.value name ] [ text label ]) options)
        ]


buildRow : Model dataModel -> (Int, GenericModel dataModel) -> Table.Row (Msg dataModel)
buildRow model (id, dataModel) =
    Table.tr [] (buildRowCells dataModel id model)


buildRowCells : GenericModel dataModel -> Int -> Model dataModel -> List (Table.Cell (Msg dataModel))
buildRowCells dataModel id model =
    List.concat
        [ [ Table.th [] [ text (String.fromInt id) ] ]
        , List.map (buildCustomRowCell dataModel) model.configuration.columns
        , List.concat
            [
                [ Table.td [ ]
                    [ Button.button
                        [ Button.info
                        , Button.onClick (UpdateRecord id)
                        ]
                        [ text "Edit" ]
                    ]
                ]
                , if model.configuration.deleteEnabled then
                    [ Table.td [ ]
                        [ Button.button
                            [ Button.danger
                            , Button.onClick (OpenDeleteModal (model.sharedConfiguration.routeGroup.existing [] id))
                            ]
                            [ text "Delete" ]
                        ]
                    ]
                else
                    []
            ]
        ]


buildCustomRowCell : GenericModel dataModel -> Column dataModel -> Table.Cell (Msg dataModel)
buildCustomRowCell model instance =
    Table.td [] [ instance.valueCallback model ]


buildPagination : Maybe (DataPage dataModel) -> List (Html (Msg dataModel))
buildPagination page =
    case page of
        Just currentPage ->
            List.concat
                [ [ case Page.previousPageNumber currentPage of
                    Just pageNumber ->
                        span [ onClick (LoadPage pageNumber) ] [ text "<" ]
                    Nothing ->
                        span [ class "inactive" ] [ text "<" ]
                ]
                , createPageLinks 1 currentPage []
                , [ case Page.nextPageNumber currentPage of
                    Just pageNumber ->
                        span [ onClick (LoadPage pageNumber) ] [ text ">" ]
                    Nothing ->
                        span [ class "inactive" ] [ text ">" ]
                ]
                ]

        Nothing ->
            []


-- @todo This is likely left over from learning, and it should be turned into some List function magic
createPageLinks : Int -> DataPage dataModel -> List (Html (Msg dataModel)) -> List (Html (Msg dataModel))
createPageLinks current page currentLinks =
    if current <= page.last_page then
        createPageLinks (current + 1) page
            <| List.concat
                [ currentLinks
                ,   [ if page.current_page == current then
                        span [ class "inactive" ] [ text (String.fromInt current) ]
                    else
                        span [ onClick (LoadPage current) ] [ text (String.fromInt current) ]
                    ]
                ]
    else
        currentLinks


-- HTTP stuff starts below

pageDecoder: Decoder (GenericModel dataModel) -> Decoder (Page.Model (GenericModel dataModel))
pageDecoder decoder =
    Page.modelDecoder <| Decode.list <| decoder


reload: Token -> Model dataModel -> Cmd (Msg dataModel)
reload token model =
    runQuery token model model.lastLoadedPage


runQuery: Token -> Model dataModel -> Int -> Cmd (Msg dataModel)
runQuery token model currentPage =
    Api.genericQuery (createQuery model currentPage) token (pageDecoder model.sharedConfiguration.decoder) NewDataResponse


createQuery: Model dataModel -> Int -> Endpoint
createQuery model currentPage =
    model.sharedConfiguration.routeGroup.index
        <| List.concat
            [ Expands.toQueryParameters model.sharedConfiguration.expands
            , List.filterMap (\field -> SearchField.buildSearchFieldQuery field.searchField) model.configuration.columns
            , [ UrlBuilder.int "page" currentPage ]
            , Order.toQueryParameters model.configuration.orders
            ]
