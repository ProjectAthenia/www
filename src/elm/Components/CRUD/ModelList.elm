module Components.CRUD.ModelList exposing (..)

import Api exposing (deleteModel, Token)
import Api.Endpoint exposing (Endpoint)
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Browser.Navigation as Navigation
import Components.LoadingIndicator as LoadingIndicator
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import List.Extra as ListExtra
import Modals.Confirmation as ConfirmationModal
import Models.Page as Page
import Models.Status as Status
import Url.Builder as UrlBuilder exposing (QueryParameter)
import Utilities.Expands as Expands
import Utilities.SearchField as SearchField


type alias DeleteResult =
    Result Api.Error Status.Model


type alias QueryResult dataModel =
    Result Api.Error (Page.Model dataModel)


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
    , valueCallback: dataModel -> Html (Msg dataModel)
    , searchField: SearchField.Model
    }


type alias Configuration dataModel =
    { pageUrl: String
    , indexEndpoint: (List QueryParameter -> Endpoint)
    , resourceName: String
    , expandFields: List Expands.Expand
    , decoder: Decoder (Page.Model dataModel)
    , dataProcessor: DataProcessor dataModel
    , columns: List (Column dataModel)
    , createDisabled: Bool
    , deleteEndpoint: Maybe (Int -> Endpoint)
    }


type alias Model dataModel =
    { navigationKey: Navigation.Key
    , loading: Bool
    , columns: List (Column dataModel)
    , models: List (Int, dataModel)
    , page: Maybe (Page.Model dataModel)
    , resourceName: String
    , pageUrl: String
    , expandFields: List Expands.Expand
    , decoder: Decoder (Page.Model dataModel)
    , dataProcessor: DataProcessor dataModel
    , createDisabled: Bool
    , indexEndpoint: List QueryParameter -> Endpoint
    , deleteEndpoint: Maybe (Int -> Endpoint)
    , deleteModal: Maybe (ConfirmationModal.Model (Msg dataModel))
    , lastLoadedPage: Int
    }


type alias DataProcessor dataModel
    = (List dataModel -> List (Int, dataModel))


-- All configuration starts here

configure: String -> (List QueryParameter -> Endpoint) -> String -> List Expands.Expand
    -> Decoder (Page.Model dataModel) -> DataProcessor dataModel -> List (Column dataModel) -> Configuration dataModel
configure pageUrl indexEndpoint resourceName expandFields decoder dataProcessor columns =
    { pageUrl = pageUrl
    , indexEndpoint = indexEndpoint
    , resourceName = resourceName
    , expandFields = expandFields
    , decoder = decoder
    , dataProcessor = dataProcessor
    , columns = columns
    , createDisabled = False
    , deleteEndpoint = Nothing
    }


disableCreate : Configuration dataModel -> Configuration dataModel
disableCreate model =
    { model
        | createDisabled = True
    }


enableDelete : Configuration dataModel -> (Int -> Endpoint) -> Configuration dataModel
enableDelete model endpoint =
    { model
        | deleteEndpoint = Just endpoint
    }


-- All state manipulations happens here

column : String -> (dataModel -> Html (Msg dataModel)) -> String -> SearchField.SearchFieldType -> Column dataModel
column header callback searchField searchFieldType =
    { header = header
    , valueCallback = callback
    , searchField = SearchField.model searchField searchFieldType
    }


initialState : Navigation.Key -> Token -> Configuration dataModel -> (Model dataModel, Cmd (Msg dataModel))
initialState navigationKey token configuration =
    let
        model =
            { navigationKey = navigationKey
            , loading = True
            , columns = configuration.columns
            , models = []
            , page = Nothing
            , resourceName = configuration.resourceName
            , pageUrl = configuration.pageUrl
            , expandFields = configuration.expandFields
            , decoder = configuration.decoder
            , dataProcessor = configuration.dataProcessor
            , createDisabled = configuration.createDisabled
            , indexEndpoint = configuration.indexEndpoint
            , deleteEndpoint = configuration.deleteEndpoint
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
            , Navigation.pushUrl model.navigationKey ("/admin/" ++ model.pageUrl ++  "/create")
            )

        OpenDeleteModal endpoint ->
            ( { model
                | deleteModal = Just
                    <| ConfirmationModal.showModal
                    <| ConfirmationModal.initialState ("Are you sure you want to delete this " ++ model.resourceName ++ "?") "This could be a real pain in the ass to undo." (ConfirmDeletion endpoint) CancelDeletion
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
            , Navigation.pushUrl model.navigationKey ("/admin/" ++ model.pageUrl ++  "/" ++ (String.fromInt id))
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
                , models = model.dataProcessor dataPage.data
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
                    [ text ("Mange " ++ model.resourceName ++ "s")
                    , if model.createDisabled == False then
                        Button.button
                            [ Button.primary
                            , Button.onClick CreateNewRecord
                            , Button.attrs [ class "create_btn" ]
                            ]
                            [ text ("Add " ++ model.resourceName) ]
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
        , List.map builderHeaderCell model.columns
        , case model.deleteEndpoint of
            Just _ ->
                [ Table.th [] []
                , Table.th [] []
                ]
            Nothing ->
                [ Table.th [] []
                ]
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

             SearchField.Select options ->
                Select.select
                    [ Select.onChange (SetSearchFieldValue instance.searchField)
                    ]
                    (List.map (\(name, label) -> Select.item [ Attributes.value name ] [ text label ]) options)
        ]


buildRow : Model dataModel -> (Int, dataModel) -> Table.Row (Msg dataModel)
buildRow indexModel (id, model) =
    Table.tr [] (buildRowCells model id indexModel)


buildRowCells : dataModel -> Int -> Model dataModel -> List (Table.Cell (Msg dataModel))
buildRowCells model id itemsModel =
    List.concat
        [ [ Table.th [] [ text (String.fromInt id) ] ]
        , List.map (buildCustomRowCell model) itemsModel.columns
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
                , case itemsModel.deleteEndpoint of
                    Just endpoint ->
                        [ Table.td [ ]
                            [ Button.button
                                [ Button.danger
                                , Button.onClick (OpenDeleteModal (endpoint id))
                                ]
                                [ text "Delete" ]
                            ]
                        ]
                    Nothing ->
                        []
            ]
        ]


buildCustomRowCell : dataModel -> Column dataModel -> Table.Cell (Msg dataModel)
buildCustomRowCell model instance =
    Table.td [] [ instance.valueCallback model ]


buildPagination : Maybe (Page.Model dataModel) -> List (Html (Msg dataModel))
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
createPageLinks : Int -> Page.Model dataModel -> List (Html (Msg dataModel)) -> List (Html (Msg dataModel))
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

reload: Token -> Model dataModel -> Cmd (Msg dataModel)
reload token model =
    runQuery token model model.lastLoadedPage

runQuery: Token -> Model dataModel -> Int -> Cmd (Msg dataModel)
runQuery token model currentPage =
    LingwaveApi.genericQuery (createQuery model currentPage) token model.decoder NewDataResponse


createQuery: Model dataModel -> Int -> Endpoint
createQuery model currentPage =
    model.indexEndpoint
        <| List.concat
            [ Expands.toQueryParameters model.expandFields
            , List.filterMap (\field -> SearchField.buildSearchFieldQuery field.searchField) model.columns
            , [ UrlBuilder.int "page" currentPage ]
            ]
