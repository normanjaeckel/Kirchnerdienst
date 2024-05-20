app [main, Model] {
    webserver: platform "https://github.com/ostcar/kingfisher/releases/download/v0.0.2/zlcCcrA9VofMd0mIrHEGwktucdmnq7xPIOuVNgY8EjE.tar.br",
    html: "vendor/roc-html/src/main.roc", # https://github.com/Hasnep/roc-html/releases/tag/v0.6.0
    json: "vendor/roc-json/package/main.roc", # https://github.com/lukewilliamboswell/roc-json/releases/tag/0.10.0
}

import webserver.Webserver exposing [Request, Response]
import html.Attribute exposing [attribute, class, name, style, type, value]
import html.Html exposing [Node, button, form, h1, h2, input, p, renderWithoutDocType, section, span, text]
import json.Json
import "index.html" as index : Str
import "assets/styles.css" as styles : List U8
import "assets/htmx/htmx.min.js" as htmx : List U8
import "assets/_hyperscript/_hyperscript.min.js" as hyperscript : List U8

Program : {
    decodeModel : [Init, Existing (List U8)] -> Result Model Str,
    encodeModel : Model -> List U8,
    handleReadRequest : Request, Model -> Response,
    handleWriteRequest : Request, Model -> (Response, Model),
}

Model : List Service

Service : {
    id : Str,
    datetime : Datetime,
    location : Str,
    description : Str,
    pastor : Str,
    assistant : Str,
    reader : Str,
}

Datetime : {
    year : U64,
    month : U8,
    day : U8,
    hour : U8,
    minute : U8,
}

main : Program
main = { decodeModel, encodeModel, handleReadRequest, handleWriteRequest }

decodeModel : [Init, Existing (List U8)] -> Result Model Str
decodeModel = \fromPlatform ->
    when fromPlatform is
        Init ->
            Ok []

        Existing encoded ->
            Decode.fromBytes encoded Json.utf8
            |> Result.mapErr \_ -> "Error: Can not decode snapshot."

encodeModel : Model -> List U8
encodeModel = \model ->
    Encode.toBytes model Json.utf8

handleReadRequest : Request, Model -> Response
handleReadRequest = \request, model ->
    isHxRequest =
        (request.headers |> List.contains { name: "Hx-Request", value: "true" |> Str.toUtf8 })
        &&
        !(request.headers |> List.contains { name: "Hx-History-Restore-Request", value: "true" |> Str.toUtf8 })

    if isHxRequest then
        when request.url |> Str.split "/" is
            ["", ""] ->
                listView model |> response200

            ["", "person-form-assistant", serviceId] ->
                updatePersonForm Assistant serviceId |> response200

            ["", "person-form-reader", serviceId] ->
                updatePersonForm Reader serviceId |> response200

            _ -> response404
    else
        when request.url |> Str.split "/" is
            ["", "assets", .. as subPath] ->
                serveAssets subPath

            _ -> response200 index

serveAssets : List Str -> Response
serveAssets = \path ->
    when path is
        ["styles.css"] ->
            { body: styles, headers: [{ name: "Content-Type", value: "text/css" |> Str.toUtf8 }], status: 200 }

        ["htmx", "htmx.min.js"] ->
            { body: htmx, headers: [{ name: "Content-Type", value: "text/javascript" |> Str.toUtf8 }], status: 200 }

        ["_hyperscript", "_hyperscript.min.js"] ->
            { body: hyperscript, headers: [{ name: "Content-Type", value: "text/javascript" |> Str.toUtf8 }], status: 200 }

        _ ->
            { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

handleWriteRequest : Request, Model -> (Response, Model)
handleWriteRequest = \request, model ->
    when request.url |> Str.split "/" is
        ["", "update-services"] ->
            when updateServices request.body model is
                Err (BadRequest msg) ->
                    (response400 msg, model)

                Ok newModel ->
                    (listView newModel |> response200, newModel)

        ["", "person-form-assistant", serviceId] ->
            when updatePersonPerform Assistant serviceId request.body model is
                Err (BadRequest msg) ->
                    (response400 msg, model)

                Ok newModel ->
                    (listView newModel |> response200, newModel)

        ["", "person-form-reader", serviceId] ->
            when updatePersonPerform Reader serviceId request.body model is
                Err (BadRequest msg) ->
                    (response400 msg, model)

                Ok newModel ->
                    (listView newModel |> response200, newModel)

        _ ->
            (response400 "$(request.url) not found", model)

Staff : [Assistant, Reader]

staffToString : Staff -> Str
staffToString = \s ->
    when s is
        Assistant -> "assistant"
        Reader -> "reader"

# ListView

listView : Model -> Str
listView = \model ->
    node =
        Html.main
            []
            (
                [h1 [] [text "Kirchnerliste"]]
                |> List.concat (model |> List.map serviceLine)
            )
    renderWithoutDocType node

serviceLine : Service -> Node
serviceLine = \service ->
    section [] [
        h2 [] [text "$(dateToStr service.datetime) · $(timeToStr service.datetime) · $(service.location)"],
        p [] [text service.description],
        p [] [text "Leitung: $(service.pastor)"],
        p [class "person-line"] [
            span [class "person"] [text "Kirchner/in: ", buttonOrName Assistant service.id service.assistant],
            span [class "person"] [text "Lektor/in: ", buttonOrName Reader service.id service.reader],
        ],
    ]

buttonOrName : Staff, Str, Str -> Node
buttonOrName = \staff, serviceId, person ->
    if person == "" then
        button
            [
                (attribute "hx-get") "/person-form-$(staffToString staff)/$(serviceId)",
                (attribute "hx-swap") "outerHTML",
            ]
            [text "Noch frei"]
    else
        span [] [span [style "margin-right:0.5em;"] [text person], button [] [text "Bearbeiten"]]

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
                    (assistant, reader) =
                        model
                        |> List.findFirst
                            \service -> service.id == entry.veranstaltung.id
                        |> Result.map \service -> (service.assistant, service.reader)
                        |> Result.withDefault ("", "")
                    {
                        id: entry.veranstaltung.id,
                        datetime: transformDatetime entry.veranstaltung.start,
                        location: entry.veranstaltung.place,
                        description: "$(entry.veranstaltung.title): $(entry.veranstaltung.subtitle)",
                        pastor: entry.veranstaltung.pastor,
                        assistant: assistant,
                        reader: reader,
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

# Update person

updatePersonForm : Staff, Str -> Str
updatePersonForm = \staff, serviceId ->
    node =
        form
            [
                (attribute "hx-post") "/person-form-$(staffToString staff)/$(serviceId)",
                (attribute "hx-target") "#mainContent",
                class "person-form",
            ]
            [
                input [type "text", name "person"],
                input [type "submit", value "Speichern"],
                button
                    [
                        (attribute "hx-get") "/",
                        (attribute "hx-target") "#mainContent",
                    ]
                    [text "Abbrechen"],
            ]
    renderWithoutDocType node

updatePersonPerform : Staff, Str, List U8, Model -> Result Model [BadRequest Str]
updatePersonPerform = \staff, serviceId, body, model ->
    bodyToFields body
    |> Result.try parsePerson
    |> Result.try
        \personName ->
            model
            |> List.map
                \service ->
                    if service.id == serviceId then
                        when staff is
                            Assistant -> { service & assistant: personName }
                            Reader -> { service & reader: personName }
                    else
                        service
            |> Ok
    |> Result.mapErr
        \err ->
            when err is
                InvalidInput msg -> BadRequest msg

parsePerson : List (Str, List U8) -> Result Str [InvalidInput Str]
parsePerson = \fields ->
    fields
    |> getField "person"
    |> Result.mapErr \_ -> InvalidInput "assistant not found in body"

# newServiceForm : Str
# newServiceForm =
#     node =
#         form
#             [
#                 (attribute "hx-post") "/new-service",
#                 (attribute "hx-target") "#mainContent",
#             ]
#             [
#                 p [] [
#                     label [] [
#                         text "Uhrzeit:",
#                         input [type "datetime-local", name "datetime", required ""],
#                     ],
#                 ],
#                 p [] [
#                     label [] [
#                         text "Ort:",
#                         input [type "text", name "location", required ""],
#                     ],
#                 ],
#                 p [] [
#                     label [] [
#                         text "Text:",
#                         textarea [name "description", rows "4", required ""] [],
#                     ],
#                 ],
#                 p [] [
#                     label [] [
#                         text "Pfarrer/in:",
#                         input [type "text", name "pastor"],
#                     ],
#                 ],
#                 p [] [
#                     input [type "submit", value "Speichern"],
#                     button
#                         [
#                             (attribute "hx-get") "/",
#                             (attribute "hx-target") "#mainContent",
#                         ]
#                         [text "Abbrechen"],
#                 ],
#             ]
#     renderWithoutDocType node

# newServicePerform : RequestBody, Model -> Result (Str, Model) [BadRequest]
# newServicePerform = \body, model ->
#     when body is
#         EmptyBody ->
#             Err BadRequest

#         Body b ->
#             bodyToFields b.body
#             |> Result.try newServiceParseFields
#             |> Result.try
#                 \newService ->
#                     newModel =
#                         model
#                         |> List.append {
#                             datetime: newService.datetime,
#                             location: newService.location,
#                             description: newService.description,
#                             pastor: newService.pastor,
#                             assistant: "",
#                             reader: "",
#                         }
#                     Ok (listView newModel, newModel)
#             |> Result.mapErr
#                 \err ->
#                     when err is
#                         InvalidInput -> BadRequest

# newServiceParseFields : List (Str, List U8) -> Result { datetime : Datetime, location : Str, description : Str, pastor : Str } [InvalidInput]
# newServiceParseFields = \fields ->
#     dt = fields |> getField "datetime"
#     loc = fields |> getField "location"
#     desc = fields |> getField "description"
#     pas = fields |> getField "pastor"

#     when (dt, loc, desc, pas) is
#         (Ok datetimeStr, Ok location, Ok description, Ok pastor) ->
#             if datetimeStr == "" || location == "" || description == "" then
#                 Err InvalidInput
#             else
#                 parseDatetime datetimeStr
#                 |> Result.try
#                     \datetime -> Ok { datetime, location, description, pastor }

#         _ -> Err InvalidInput

# parseDatetime : Str -> Result Datetime [InvalidInput]
# parseDatetime = \datetime ->
#     when datetime |> Str.split "T" is
#         [date, time] ->
#             when (date |> Str.split "-", time |> Str.split ":") is
#                 ([y, mo, d], [h, mi]) ->
#                     when (Str.toU64 y, Str.toU8 mo, Str.toU8 d, Str.toU8 h, Str.toU8 mi) is
#                         (Ok year, Ok month, Ok day, Ok hour, Ok minute) ->
#                             Ok { year, month, day, hour, minute }

#                         _ -> Err InvalidInput

#                 _ -> Err InvalidInput

#         _ -> Err InvalidInput

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
    |> urlDecode
    |> Result.try
        \r ->
            r
            |> splitListU8 '&'
            |> List.mapTry
                \elem ->
                    when elem |> splitListU8 '=' is
                        [elemName, elemValue] ->
                            when elemName |> Str.fromUtf8 is
                                Ok n ->
                                    Ok (n, elemValue)

                                Err (BadUtf8 _ _) ->
                                    Err (InvalidInput "Can not decode some key")

                        _ -> Err (InvalidInput "Can not split up key-value pairs at equal sign")

expect
    got = bodyToFields ("foo=bar&val=baz" |> Str.toUtf8)
    got == Ok ([("foo", "bar" |> Str.toUtf8), ("val", "baz" |> Str.toUtf8)])

expect
    got = bodyToFields ("invalid&val=baz" |> Str.toUtf8)
    got == Err (InvalidInput "Can not split up key-value pairs at equal sign")

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
