app "kirchnerdienst"
    packages {
        webserver: "https://oshahn.de/kQ7c0Gc2pfoaCN7t7oXhJWld1JlZcjlUgVjZ1ccwxis.tar.br",
        html: "vendor/roc-html/src/main.roc", # html : "https://github.com/Hasnep/roc-html/releases/download/v0.4.0/sS6DMu08ogvM7j5S4E-A6VwdwQiVPlh6DbrTHbBAhZw.tar.br",
        json: "vendor/roc-json/package/main.roc", # json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.7.0/xuaMzXRVG_SEhOFZucS3iBozlRdObWsfKaYZMHVE_q0.tar.br",
    }
    imports [
        webserver.Webserver.{ Request, RequestBody, Response },
        html.Attribute.{ attribute, id, name, required, rows, type, value },
        html.Html.{ Node, button, form, h1, h2, input, label, p, renderWithoutDocType, section, span, text, textarea },
        json.Core.{ json },
        "index.html" as index : Str,
        "assets/styles.css" as styles : List U8,
        "assets/htmx/htmx.min.js" as htmx : List U8,
    ]
    provides [main, Model] to webserver

Program : {
    decodeModel : [Init, Existing (List U8)] -> Model,
    encodeModel : Model -> List U8,
    handleReadRequest : Request, Model -> Response,
    handleWriteRequest : Request, Model -> (Response, Model),
}

Model : List Service

Service : {
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

decodeModel : [Init, Existing (List U8)] -> Model
decodeModel = \fromPlatform ->
    when fromPlatform is
        Init ->
            []

        Existing encoded ->
            when Decode.fromBytes encoded json is
                Ok model -> model
                Err _ -> crash "Error: Can not decode snapshot."

encodeModel : Model -> List U8
encodeModel = \model ->
    Encode.toBytes model json

handleReadRequest : Request, Model -> Response
handleReadRequest = \request, model ->
    isHxRequest =
        (request.headers |> List.contains { name: "Hx-Request", value: "true" })
        &&
        !(request.headers |> List.contains { name: "Hx-History-Restore-Request", value: "true" })

    if isHxRequest then
        when request.url |> Str.split "/" is
            ["", ""] ->
                listView model |> response200

            ["", "new-service"] ->
                newServiceForm |> response200

            _ -> response404
    else
        when request.url |> Str.split "/" is
            ["", "assets", .. as subPath] ->
                serveAssets subPath

            # TODO: ["", "new-service"] -> ...
            _ -> index |> Str.replaceFirst "{% contentPath %}" request.url |> response200

serveAssets : List Str -> Response
serveAssets = \path ->
    when path is
        ["styles.css"] ->
            { body: styles, headers: [{ name: "Content-Type", value: "text/css" }], status: 200 }

        ["htmx", "htmx.min.js"] ->
            { body: htmx, headers: [{ name: "Content-Type", value: "text/javascript" }], status: 200 }

        _ ->
            { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

handleWriteRequest : Request, Model -> (Response, Model)
handleWriteRequest = \request, model ->
    when request.url |> Str.split "/" is
        ["", "new-service"] ->
            when newServicePerform request.body model is
                Err BadRequest ->
                    (response400, model)

                Ok (responseBody, newModel) ->
                    (response200 responseBody, newModel)

        _ ->
            (response400, model)

# ListView

listView : Model -> Str
listView = \model ->
    node =
        Html.main
            []
            (
                [h1 [] [text "Kirchnerliste"]]
                |> List.concat (model |> List.map serviceLine)
                |> List.append
                    (
                        section [id "new-service-button"] [
                            button
                                [
                                    (attribute "hx-get") "/new-service",
                                    (attribute "hx-target") "#new-service-button",
                                ]
                                [text "Neuer Eintrag"],
                        ]
                    )
            )
    renderWithoutDocType node

serviceLine : Service -> Node
serviceLine = \service ->
    assistantFreeButton =
        if service.assistant == "" then
            [button [] [text "Noch frei"]]
        else
            []

    readerFreeButton =
        if service.reader == "" then
            [button [] [text "Noch frei"]]
        else
            []

    section [] [
        h2 [] [
            span [] [text "$(dateToStr service.datetime) · $(timeToStr service.datetime) · $(service.location)"],
            button [] [text "Bearbeiten"],
            button [] [text "Löschen"],
        ],
        p [] [text service.description],
        p [] [text "Leitung: $(service.pastor)"],
        p [] ([text "Kirchner/in: $(service.assistant)"] |> List.concat assistantFreeButton),
        p [] ([text "Lektor/in: $(service.reader)"] |> List.concat readerFreeButton),
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

# New service

newServiceForm : Str
newServiceForm =
    node =
        form
            [
                (attribute "hx-post") "/new-service",
                (attribute "hx-target") "#mainContent",
            ]
            [
                p [] [
                    label [] [
                        text "Uhrzeit:",
                        input [type "datetime-local", name "datetime", required ""],
                    ],
                ],
                p [] [
                    label [] [
                        text "Ort:",
                        input [type "text", name "location", required ""],
                    ],
                ],
                p [] [
                    label [] [
                        text "Text:",
                        textarea [name "description", rows "4", required ""] [],
                    ],
                ],
                p [] [
                    label [] [
                        text "Pfarrer/in:",
                        input [type "text", name "pastor"],
                    ],
                ],
                p [] [
                    input [type "submit", value "Speichern"],
                    button
                        [
                            (attribute "hx-get") "/",
                            (attribute "hx-target") "#mainContent",
                        ]
                        [text "Abbrechen"],
                ],
            ]
    renderWithoutDocType node

newServicePerform : RequestBody, Model -> Result (Str, Model) [BadRequest]
newServicePerform = \body, model ->
    when body is
        EmptyBody ->
            Err BadRequest

        Body b ->
            bodyToFields b.body
            |> Result.try newServiceParseFields
            |> Result.try
                \newService ->
                    newModel =
                        model
                        |> List.append {
                            datetime: newService.datetime,
                            location: newService.location,
                            description: newService.description,
                            pastor: newService.pastor,
                            assistant: "",
                            reader: "",
                        }
                    Ok (listView newModel, newModel)
            |> Result.mapErr
                \err ->
                    when err is
                        InvalidInput -> BadRequest

newServiceParseFields : List (Str, List U8) -> Result { datetime : Datetime, location : Str, description : Str, pastor : Str } [InvalidInput]
newServiceParseFields = \fields ->
    dt = fields |> getField "datetime"
    loc = fields |> getField "location"
    desc = fields |> getField "description"
    pas = fields |> getField "pastor"

    when (dt, loc, desc, pas) is
        (Ok datetimeStr, Ok location, Ok description, Ok pastor) ->
            if datetimeStr == "" || location == "" || description == "" then
                Err InvalidInput
            else
                parseDatetime datetimeStr
                |> Result.try
                    \datetime -> Ok { datetime, location, description, pastor }

        _ -> Err InvalidInput

parseDatetime : Str -> Result Datetime [InvalidInput]
parseDatetime = \datetime ->
    when datetime |> Str.split "T" is
        [date, time] ->
            when (date |> Str.split "-", time |> Str.split ":") is
                ([y, mo, d], [h, mi]) ->
                    when (Str.toU64 y, Str.toU8 mo, Str.toU8 d, Str.toU8 h, Str.toU8 mi) is
                        (Ok year, Ok month, Ok day, Ok hour, Ok minute) ->
                            Ok { year, month, day, hour, minute }

                        _ -> Err InvalidInput

                _ -> Err InvalidInput

        _ -> Err InvalidInput

# Shared

response200 : Str -> Response
response200 = \body ->
    { body: body |> Str.toUtf8, headers: [], status: 200 }

response400 : Response
response400 =
    { body: "400 Bad Request" |> Str.toUtf8, headers: [], status: 400 }

response404 : Response
response404 =
    { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

bodyToFields : List U8 -> Result (List (Str, List U8)) [InvalidInput]
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
                                    Err InvalidInput

                        _ -> Err InvalidInput

expect
    got = bodyToFields ("foo=bar&val=baz" |> Str.toUtf8)
    got == Ok ([("foo", "bar" |> Str.toUtf8), ("val", "baz" |> Str.toUtf8)])

expect
    got = bodyToFields ("invalid&val=baz" |> Str.toUtf8)
    got == Err InvalidInput

urlDecode : List U8 -> Result (List U8) [InvalidInput]
urlDecode = \bytes ->
    bytes
    |> List.map
        \char -> if char == '+' then ' ' else char
    |> percentDecode

percentDecode : List U8 -> Result (List U8) [InvalidInput]
percentDecode = \bytes ->
    percentDecodeHelper bytes (List.withCapacity (List.len bytes))

percentDecodeHelper : List U8, List U8 -> Result (List U8) [InvalidInput]
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
                            BadUtf8 _ _ | InvalidNumStr -> Err InvalidInput
            else
                percentDecodeHelper rest (result |> List.append first)

expect urlDecode ("foo%20bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect urlDecode ("foo+bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect urlDecode ("foo%" |> Str.toUtf8) == Err InvalidInput
expect urlDecode ("foo%zz" |> Str.toUtf8) == Err InvalidInput

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

getField : List (Str, List U8), Str -> Result Str [InvalidInput, NotFound]
getField = \fields, fieldName ->
    fields
    |> List.findFirst \(element, _) -> element == fieldName
    |> Result.try \(_, val) -> val |> Str.fromUtf8
    |> Result.mapErr
        \err ->
            when err is
                BadUtf8 _ _ -> InvalidInput
                NotFound -> NotFound

expect getField [("field", "value" |> Str.toUtf8)] "field" == Ok "value"
expect getField [("field", "value" |> Str.toUtf8)] "other-field" == Err NotFound
