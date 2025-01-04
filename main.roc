app [init_model, update_model, handle_request!, Model] {
    # webserver: platform "https://github.com/ostcar/kingfisher/releases/download/...",
    webserver: platform "vendor/kingfisher/platform/main.roc",
    html: "vendor/roc-html/src/main.roc", # https://github.com/Hasnep/roc-html/releases/tag/v0.6.0
    json: "vendor/roc-json/package/main.roc", # https://github.com/lukewilliamboswell/roc-json/releases/tag/0.11.0
}

import webserver.Http exposing [Request, Response]
import html.Attribute exposing [attribute, class, name, rows, style, type, value]
import html.Html exposing [Node, button, div, form, h1, h2, input, p, renderWithoutDocType, section, span, text, textarea]
import json.Json
import "index.html" as index : Str
import "assets/styles.css" as styles : List U8
import "assets/htmx/htmx.min.js" as htmx : List U8
import "assets/_hyperscript/_hyperscript.min.js" as hyperscript : List U8

Model : List Service

Service : {
    id : Str,
    datetime : Datetime,
    location : Str,
    description : Str,
    pastor : Str,
    assistant : Str,
    reader : Str,
    notes : Str,
}

Datetime : {
    year : U64,
    month : U8,
    day : U8,
    hour : U8,
    minute : U8,
}

init_model : Model
init_model =
    []

update_model : Model, List (List U8) -> Result Model [ListWasEmpty, DecodeError]
update_model = \_model, events ->
    when events |> List.last is
        Err ListWasEmpty -> Ok init_model
        Ok event -> event |> decodeModel

handle_request! : Request, Model => Result Response _
handle_request! = \request, model ->
    when request.method is
        Get ->
            handleReadRequest request model |> Ok

        Post save_event! ->
            (resp, new_model) = handleWriteRequest request model
            save_event! (encode_model new_model)
            Ok resp

        _ ->
            Err (SomeErr "Some error string")

decodeModel : List U8 -> Result Model [DecodeError]
decodeModel = \encoded ->
    Decode.fromBytes encoded Json.utf8
    |> Result.mapErr \_ -> DecodeError

encode_model : Model -> List U8
encode_model = \model ->
    Encode.toBytes model Json.utf8

handleReadRequest : Request, Model -> Response
handleReadRequest = \request, model ->
    isHxRequest =
        (request.headers |> List.contains { name: "Hx-Request", value: "true" })
        &&
        !(request.headers |> List.contains { name: "Hx-History-Restore-Request", value: "true" })

    if isHxRequest then
        when request.url |> Str.splitOn "/" is
            ["", ""] ->
                listView model |> response200

            ["", "info-form", info, serviceId] ->
                service = model |> List.findFirst \s -> s.id == serviceId
                when (stringToInfo info, service) is
                    (Ok inf, Ok serv) ->
                        updateInfoForm inf serviceId serv |> response200

                    _ -> response404

            _ -> response404
    else
        when request.url |> Str.splitOn "/" is
            ["", "assets", .. as subPath] ->
                serveAssets subPath

            _ -> response200 index

serveAssets : List Str -> Response
serveAssets = \path ->
    when path is
        ["styles.css"] ->
            { body: styles, headers: [{ name: "Content-Type", value: "text/css" }], status: 200 }

        ["htmx", "htmx.min.js"] ->
            { body: htmx, headers: [{ name: "Content-Type", value: "text/javascript" }], status: 200 }

        ["_hyperscript", "_hyperscript.min.js"] ->
            { body: hyperscript, headers: [{ name: "Content-Type", value: "text/javascript" }], status: 200 }

        _ ->
            { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

handleWriteRequest : Request, Model -> (Response, Model)
handleWriteRequest = \request, model ->
    when request.url |> Str.splitOn "/" is
        ["", "update-services"] ->
            when updateServices request.body model is
                Err (BadRequest msg) ->
                    (response400 msg, model)

                Ok newModel ->
                    (listView newModel |> response200, newModel)

        ["", "info-form", info, serviceId] ->
            when stringToInfo info is
                Ok inf ->
                    when updateInfoPerform inf serviceId request.body model is
                        Err (BadRequest msg) ->
                            (response400 msg, model)

                        Ok newModel ->
                            (listView newModel |> response200, newModel)

                Err NotFound -> (response400 "$(request.url) not found", model)

        _ ->
            (response400 "$(request.url) not found", model)

Info : [Assistant, Reader, Notes]

infoToString : Info -> Str
infoToString = \s ->
    when s is
        Assistant -> "assistant"
        Reader -> "reader"
        Notes -> "notes"

stringToInfo : Str -> Result Info [NotFound]
stringToInfo = \s ->
    when s is
        "assistant" -> Ok Assistant
        "reader" -> Ok Reader
        "notes" -> Ok Notes
        _ -> Err NotFound

# ListView

listView : Model -> Str
listView = \model ->
    node =
        Html.main
            []
            (
                [
                    h1 [] [text "Kirchnerliste"],
                    section [] [
                        h2 [] [text "Erläuterungen"],
                        p [] [text "Wenn bei Lektor/in niemand eingetragen ist, übernimmt der/die Kirchner/in auch die Lesungen."],
                        p [] [text "Abkürzungen: +AM = mit Abendmahl, +Kur = mit Kurrende, +Chor = mit Kantorei, +Pos = mit Posaunenchor, +KiGo = mit Kindergottesdienst, +KiKa = anschließend Kirchenkaffee, +FamBra = anschließend Familienbrunch"],
                    ],
                ]
                |> List.concat (model |> List.map serviceLine)
            )
    renderWithoutDocType node

serviceLine : Service -> Node
serviceLine = \service ->
    section [] [
        h2 [] [text "$(dateToStr service.datetime) · $(timeToStr service.datetime) · $(service.location)"],
        p [] [text service.description],
        p [] [text "Leitung: $(service.pastor)"],
        div [class "person-line"] [
            p [class "info"] [text "Kirchner/in: ", buttonOrText Assistant service.id service.assistant],
            p [class "info"] [text "Lektor/in: ", buttonOrText Reader service.id service.reader],
        ],
        p [class "info"] [text "Bemerkungen: ", buttonOrText Notes service.id service.notes],
    ]

buttonOrText : Info, Str, Str -> Node
buttonOrText = \info, serviceId, infoText ->
    if infoText == "" then
        button
            [
                (attribute "hx-get") "/info-form/$(infoToString info)/$(serviceId)",
                (attribute "hx-swap") "outerHTML",
            ]
            [text "Noch frei"]
    else
        span [] [
            span [style "margin-right:0.5em;"] [text infoText],
            button
                [
                    (attribute "hx-get") "/info-form/$(infoToString info)/$(serviceId)",
                    (attribute "hx-target") "closest .info span",
                    (attribute "hx-swap") "outerHTML",
                ]
                [text "Bearbeiten"],
        ]

dateToStr : Datetime -> Str
dateToStr = \datetime ->
    "$(numToStrWithZero datetime.day).$(numToStrWithZero datetime.month).$(Num.toStr datetime.year)"

timeToStr : Datetime -> Str
timeToStr = \datetime ->
    "$(numToStrWithZero datetime.hour):$(numToStrWithZero datetime.minute) Uhr"

numToStrWithZero : Num * -> Str
numToStrWithZero = \n ->
    if
        n < 10
    then
        "0$(Num.toStr n)"
    else
        Num.toStr n

# Update services

updateServices : List U8, Model -> Result Model [BadRequest Str]
updateServices = \body, model ->
    bodyToFields body
    |> Result.try parseCalendarEntries
    |> Result.try
        \calenderEntries ->
            calenderEntries
            |> List.map
                \entry ->
                    (assistant, reader, notes) =
                        model
                        |> List.findFirst
                            \service -> service.id == entry.veranstaltung.id
                        |> Result.map \service -> (service.assistant, service.reader, service.notes)
                        |> Result.withDefault ("", "", "")
                    {
                        id: entry.veranstaltung.id,
                        datetime: transformDatetime entry.veranstaltung.start,
                        location: entry.veranstaltung.place,
                        description: "$(entry.veranstaltung.title): $(entry.veranstaltung.subtitle)",
                        pastor: entry.veranstaltung.pastor,
                        assistant: assistant,
                        reader: reader,
                        notes: notes,
                    }
            |> Ok
    |> Result.mapErr
        \err ->
            when err is
                InvalidInput msg -> BadRequest msg

CalendarObject : {
    veranstaltung : {
        id : Str,
        title : Str,
        subtitle : Str,
        pastor : Str,
        start : Str,
        place : Str,
    },
}

calendarFieldNameMapping : Str -> Str
calendarFieldNameMapping = \fieldName ->
    when fieldName is
        "Veranstaltung" -> "veranstaltung"
        "ID" -> "id"
        "_event_TITLE" -> "title"
        "SUBTITLE" -> "subtitle"
        "START_RFC" -> "start"
        "_person_NAME" -> "pastor"
        "_place_NAME" -> "place"
        _ -> fieldName

parseCalendarEntries : List (Str, List U8) -> Result (List CalendarObject) [InvalidInput Str]
parseCalendarEntries = \fields ->
    fields
    |> List.findFirst \(fieldName, _) -> fieldName == "data"
    |> Result.try \(_, data) -> Decode.fromBytes data (Json.utf8With { fieldNameMapping: Custom calendarFieldNameMapping })
    |> Result.try \decoded -> Ok decoded
    |> Result.mapErr
        \err ->
            when err is
                NotFound -> InvalidInput "Can not parse calendar entries: not found"
                Leftover bytes -> InvalidInput "Can not parse calendar entries: left over: $(bytes |> Str.fromUtf8 |> Result.withDefault "default")"
                TooShort -> InvalidInput "Can not parse calendar entries: too short"

expect
    s = "\"St\\u00f6tteritz\""
    got = Decode.fromBytes (s |> Str.toUtf8) Json.utf8
    got == Ok "Stötteritz"

transformDatetime : Str -> Datetime
transformDatetime = \s ->
    datetime = s |> Str.toUtf8
    year = datetime |> List.takeFirst 4 |> Str.fromUtf8 |> Result.try Str.toU64 |> Result.withDefault 0
    month = datetime |> List.sublist { start: 5, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    day = datetime |> List.sublist { start: 8, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    hour = datetime |> List.sublist { start: 11, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    minute = datetime |> List.sublist { start: 14, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    { year, month, day, hour, minute }

# Update info

updateInfoForm : Info, Str, Service -> Str
updateInfoForm = \info, serviceId, service ->
    inputField : Node
    inputField =
        when info is
            Assistant ->
                input [type "text", name "info", value service.assistant]

            Reader ->
                input [type "text", name "info", value service.reader]

            Notes ->
                textarea [name "info", rows "5", style "width:95%"] [text service.notes]

    node : Node
    node =
        form
            [
                (attribute "hx-post") "/info-form/$(infoToString info)/$(serviceId)",
                (attribute "hx-target") "#mainContent",
                class "info-form",
            ]
            [
                inputField,
                input [type "submit", value "Speichern"],
                button
                    [
                        (attribute "hx-get") "/",
                        (attribute "hx-target") "#mainContent",
                    ]
                    [text "Abbrechen"],
            ]
    renderWithoutDocType node

updateInfoPerform : Info, Str, List U8, Model -> Result Model [BadRequest Str]
updateInfoPerform = \info, serviceId, body, model ->
    bodyToFields body
    |> Result.try parseInfo
    |> Result.try
        \infoText ->
            model
            |> List.map
                \service ->
                    if service.id == serviceId then
                        when info is
                            Assistant -> { service & assistant: infoText }
                            Reader -> { service & reader: infoText }
                            Notes -> { service & notes: infoText }
                    else
                        service
            |> Ok
    |> Result.mapErr
        \err ->
            when err is
                InvalidInput msg -> BadRequest msg

parseInfo : List (Str, List U8) -> Result Str [InvalidInput Str]
parseInfo = \fields ->
    fields
    |> getField "info"
    |> Result.mapErr \_ -> InvalidInput "info not found in body"

# Shared

response200 : Str -> Response
response200 = \body ->
    { body: body |> Str.toUtf8, headers: [], status: 200 }

response400 : Str -> Response
response400 = \msg ->
    { body: "400 Bad Request: $(msg)" |> Str.toUtf8, headers: [], status: 400 }

response404 : Response
response404 =
    { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

bodyToFields : List U8 -> Result (List (Str, List U8)) [InvalidInput Str]
bodyToFields = \body ->
    body
    |> splitListU8 '&'
    |> List.mapTry
        \elem ->
            when elem |> splitListU8 '=' is
                [elemName, elemValue] ->
                    elemName
                    |> urlDecode
                    |> Result.try
                        \eName ->
                            when eName |> Str.fromUtf8 is
                                Ok n ->
                                    elemValue |> urlDecode |> Result.try \val -> Ok (n, val)

                                Err (BadUtf8 _ _) ->
                                    Err (InvalidInput "Can not decode some key")

                _ -> Err (InvalidInput "Can not split up key-value pairs at equal sign")

expect
    got = bodyToFields ("foo=bar&val=baz" |> Str.toUtf8)
    got == Ok ([("foo", "bar" |> Str.toUtf8), ("val", "baz" |> Str.toUtf8)])

expect
    got = bodyToFields ("invalid&val=baz" |> Str.toUtf8)
    got == Err (InvalidInput "Can not split up key-value pairs at equal sign")

expect
    got = bodyToFields ("foo=bar%3Dbaz" |> Str.toUtf8)
    got == Ok ([("foo", "bar=baz" |> Str.toUtf8)])

splitListU8 : List U8, U8 -> List (List U8)
splitListU8 = \list, char ->
    list
    |> List.walk
        ([], [])
        \(current, result), elem ->
            if elem == char then
                ([], result |> List.append current)
            else
                (current |> List.append elem, result)
    |> \(current, result) ->
        result |> List.append current

expect splitListU8 [] 'a' == [[]]
expect splitListU8 ['a', 'b', 'c'] 'b' == [['a'], ['c']]
expect splitListU8 ['a', 'b', 'c'] 'c' == [['a', 'b'], []]
expect splitListU8 ['a', 'b', 'c'] 'a' == [[], ['b', 'c']]
expect splitListU8 ['a', 'b', 'b', 'c'] 'b' == [['a'], [], ['c']]
expect splitListU8 ['a', 'b', 'c', 'b', 'd'] 'b' == [['a'], ['c'], ['d']]

urlDecode : List U8 -> Result (List U8) [InvalidInput Str]
urlDecode = \bytes ->
    bytes
    |> List.map
        \char -> if char == '+' then ' ' else char
    |> percentDecode

percentDecode : List U8 -> Result (List U8) [InvalidInput Str]
percentDecode = \bytes ->
    percentDecodeHelper bytes (List.withCapacity (List.len bytes))

percentDecodeHelper : List U8, List U8 -> Result (List U8) [InvalidInput Str]
percentDecodeHelper = \bytes, result ->
    when bytes is
        [] -> Ok result
        [first, .. as rest] ->
            if first == '%' then
                hex =
                    rest
                    |> List.takeFirst 2
                    |> Str.fromUtf8
                    |> Result.try
                        \s -> "0x$(s)" |> Str.toU8
                when hex is
                    Ok num ->
                        percentDecodeHelper (rest |> List.dropFirst 2) (result |> List.append num)

                    Err e ->
                        when e is
                            BadUtf8 _ _ | InvalidNumStr -> Err (InvalidInput "Can not decode percent value")
            else
                percentDecodeHelper rest (result |> List.append first)

expect urlDecode ("foo%20bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect urlDecode ("foo+bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect urlDecode ("foo%" |> Str.toUtf8) == Err (InvalidInput "Can not decode percent value")
expect urlDecode ("foo%zz" |> Str.toUtf8) == Err (InvalidInput "Can not decode percent value")

getField : List (Str, List U8), Str -> Result Str [InvalidInput Str, NotFound]
getField = \fields, fieldName ->
    fields
    |> List.findFirst \(element, _) -> element == fieldName
    |> Result.try \(_, val) -> val |> Str.fromUtf8
    |> Result.mapErr
        \err ->
            when err is
                BadUtf8 _ _ -> InvalidInput "Can not decode value of $(fieldName)"
                NotFound -> NotFound

expect getField [("field", "value" |> Str.toUtf8)] "field" == Ok "value"
expect getField [("field", "value" |> Str.toUtf8)] "other-field" == Err NotFound
