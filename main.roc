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
        Ok event -> event |> decode_model

handle_request! : Request, Model => Result Response [Error Str]
handle_request! = \request, model ->
    when request.method is
        Get ->
            handle_read_request request model |> Ok

        Post save_event! ->
            (resp, new_model) = handle_write_request request model
            save_event! (encode_model new_model)
            Ok resp

        _ ->
            Err (Error "Invalid method")

decode_model : List U8 -> Result Model [DecodeError]
decode_model = \encoded ->
    Decode.fromBytes encoded Json.utf8
    |> Result.mapErr \_ -> DecodeError

encode_model : Model -> List U8
encode_model = \model ->
    Encode.toBytes model Json.utf8

handle_read_request : Request, Model -> Response
handle_read_request = \request, model ->
    is_hx_request =
        (request.headers |> List.contains { name: "Hx-Request", value: "true" })
        &&
        !(request.headers |> List.contains { name: "Hx-History-Restore-Request", value: "true" })

    if is_hx_request then
        when request.url |> Str.splitOn "/" is
            ["", ""] ->
                list_view model |> response_200

            ["", "info-form", info, service_id] ->
                service = model |> List.findFirst \s -> s.id == service_id
                when (string_to_info info, service) is
                    (Ok inf, Ok serv) ->
                        update_info_form inf service_id serv |> response_200

                    _ -> response_404

            _ -> response_404
    else
        when request.url |> Str.splitOn "/" is
            ["", "assets", .. as sub_path] ->
                serve_assets sub_path

            _ -> response_200 index

serve_assets : List Str -> Response
serve_assets = \path ->
    when path is
        ["styles.css"] ->
            { body: styles, headers: [{ name: "Content-Type", value: "text/css" }], status: 200 }

        ["htmx", "htmx.min.js"] ->
            { body: htmx, headers: [{ name: "Content-Type", value: "text/javascript" }], status: 200 }

        ["_hyperscript", "_hyperscript.min.js"] ->
            { body: hyperscript, headers: [{ name: "Content-Type", value: "text/javascript" }], status: 200 }

        _ ->
            { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

handle_write_request : Request, Model -> (Response, Model)
handle_write_request = \request, model ->
    when request.url |> Str.splitOn "/" is
        ["", "update-services"] ->
            when update_services request.body model is
                Err (BadRequest msg) ->
                    (response_400 msg, model)

                Ok new_model ->
                    (list_view new_model |> response_200, new_model)

        ["", "info-form", info, serviceId] ->
            when string_to_info info is
                Ok inf ->
                    when update_info_perform inf serviceId request.body model is
                        Err (BadRequest msg) ->
                            (response_400 msg, model)

                        Ok new_model ->
                            (list_view new_model |> response_200, new_model)

                Err NotFound -> (response_400 "$(request.url) not found", model)

        _ ->
            (response_400 "$(request.url) not found", model)

Info : [Assistant, Reader, Notes]

info_to_string : Info -> Str
info_to_string = \s ->
    when s is
        Assistant -> "assistant"
        Reader -> "reader"
        Notes -> "notes"

string_to_info : Str -> Result Info [NotFound]
string_to_info = \s ->
    when s is
        "assistant" -> Ok Assistant
        "reader" -> Ok Reader
        "notes" -> Ok Notes
        _ -> Err NotFound

# List view

list_view : Model -> Str
list_view = \model ->
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
                |> List.concat (model |> List.map service_line)
            )
    renderWithoutDocType node

service_line : Service -> Node
service_line = \service ->
    section [] [
        h2 [] [text "$(date_to_str service.datetime) · $(time_to_str service.datetime) · $(service.location)"],
        p [] [text service.description],
        p [] [text "Leitung: $(service.pastor)"],
        div [class "person-line"] [
            p [class "info"] [text "Kirchner/in: ", button_or_text Assistant service.id service.assistant],
            p [class "info"] [text "Lektor/in: ", button_or_text Reader service.id service.reader],
        ],
        p [class "info"] [text "Bemerkungen: ", button_or_text Notes service.id service.notes],
    ]

button_or_text : Info, Str, Str -> Node
button_or_text = \info, service_id, infoText ->
    if infoText == "" then
        button
            [
                (attribute "hx-get") "/info-form/$(info_to_string info)/$(service_id)",
                (attribute "hx-swap") "outerHTML",
            ]
            [text "Noch frei"]
    else
        span [] [
            span [style "margin-right:0.5em;"] [text infoText],
            button
                [
                    (attribute "hx-get") "/info-form/$(info_to_string info)/$(service_id)",
                    (attribute "hx-target") "closest .info span",
                    (attribute "hx-swap") "outerHTML",
                ]
                [text "Bearbeiten"],
        ]

date_to_str : Datetime -> Str
date_to_str = \datetime ->
    "$(num_to_str_with_zero datetime.day).$(num_to_str_with_zero datetime.month).$(Num.toStr datetime.year)"

time_to_str : Datetime -> Str
time_to_str = \datetime ->
    "$(num_to_str_with_zero datetime.hour):$(num_to_str_with_zero datetime.minute) Uhr"

num_to_str_with_zero : Num * -> Str
num_to_str_with_zero = \n ->
    if
        n < 10
    then
        "0$(Num.toStr n)"
    else
        Num.toStr n

# Update services

update_services : List U8, Model -> Result Model [BadRequest Str]
update_services = \body, model ->
    body_to_fields body
    |> Result.try parse_calendar_entries
    |> Result.try \calendar_entries ->
        calendar_entries
        |> List.map \entry ->
            (assistant, reader, notes) =
                model
                |> List.findFirst \service -> service.id == entry.veranstaltung.id
                |> Result.map \service -> (service.assistant, service.reader, service.notes)
                |> Result.withDefault ("", "", "")
            {
                id: entry.veranstaltung.id,
                datetime: transform_datetime entry.veranstaltung.start,
                location: entry.veranstaltung.place,
                description: "$(entry.veranstaltung.title): $(entry.veranstaltung.subtitle)",
                pastor: entry.veranstaltung.pastor,
                assistant: assistant,
                reader: reader,
                notes: notes,
            }
        |> Ok
    |> Result.mapErr \err ->
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

calendar_field_name_mapping : Str -> Str
calendar_field_name_mapping = \field_name ->
    when field_name is
        "Veranstaltung" -> "veranstaltung"
        "ID" -> "id"
        "_event_TITLE" -> "title"
        "SUBTITLE" -> "subtitle"
        "START_RFC" -> "start"
        "_person_NAME" -> "pastor"
        "_place_NAME" -> "place"
        _ -> field_name

parse_calendar_entries : List (Str, List U8) -> Result (List CalendarObject) [InvalidInput Str]
parse_calendar_entries = \fields ->
    fields
    |> List.findFirst \(field_name, _) -> field_name == "data"
    |> Result.try \(_, data) -> Decode.fromBytes data (Json.utf8With { fieldNameMapping: Custom calendar_field_name_mapping })
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

transform_datetime : Str -> Datetime
transform_datetime = \s ->
    datetime = s |> Str.toUtf8
    year = datetime |> List.takeFirst 4 |> Str.fromUtf8 |> Result.try Str.toU64 |> Result.withDefault 0
    month = datetime |> List.sublist { start: 5, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    day = datetime |> List.sublist { start: 8, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    hour = datetime |> List.sublist { start: 11, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    minute = datetime |> List.sublist { start: 14, len: 2 } |> Str.fromUtf8 |> Result.try Str.toU8 |> Result.withDefault 0
    { year, month, day, hour, minute }

# Update info

update_info_form : Info, Str, Service -> Str
update_info_form = \info, serviceId, service ->
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
                (attribute "hx-post") "/info-form/$(info_to_string info)/$(serviceId)",
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

update_info_perform : Info, Str, List U8, Model -> Result Model [BadRequest Str]
update_info_perform = \info, service_id, body, model ->
    body_to_fields body
    |> Result.try parse_info
    |> Result.try
        \info_text ->
            model
            |> List.map
                \service ->
                    if service.id == service_id then
                        when info is
                            Assistant -> { service & assistant: info_text }
                            Reader -> { service & reader: info_text }
                            Notes -> { service & notes: info_text }
                    else
                        service
            |> Ok
    |> Result.mapErr
        \err ->
            when err is
                InvalidInput msg -> BadRequest msg

parse_info : List (Str, List U8) -> Result Str [InvalidInput Str]
parse_info = \fields ->
    fields
    |> get_field "info"
    |> Result.mapErr \_ -> InvalidInput "info not found in body"

# Shared

response_200 : Str -> Response
response_200 = \body ->
    { body: body |> Str.toUtf8, headers: [], status: 200 }

response_400 : Str -> Response
response_400 = \msg ->
    { body: "400 Bad Request: $(msg)" |> Str.toUtf8, headers: [], status: 400 }

response_404 : Response
response_404 =
    { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

body_to_fields : List U8 -> Result (List (Str, List U8)) [InvalidInput Str]
body_to_fields = \body ->
    body
    |> split_list_U8 '&'
    |> List.mapTry
        \elem ->
            when elem |> split_list_U8 '=' is
                [elem_name, elem_value] ->
                    elem_name
                    |> url_decode
                    |> Result.try
                        \e_name ->
                            when e_name |> Str.fromUtf8 is
                                Ok n ->
                                    elem_value |> url_decode |> Result.try \val -> Ok (n, val)

                                Err (BadUtf8 _ _) ->
                                    Err (InvalidInput "Can not decode some key")

                _ -> Err (InvalidInput "Can not split up key-value pairs at equal sign")

expect
    got = body_to_fields ("foo=bar&val=baz" |> Str.toUtf8)
    got == Ok ([("foo", "bar" |> Str.toUtf8), ("val", "baz" |> Str.toUtf8)])

expect
    got = body_to_fields ("invalid&val=baz" |> Str.toUtf8)
    got == Err (InvalidInput "Can not split up key-value pairs at equal sign")

expect
    got = body_to_fields ("foo=bar%3Dbaz" |> Str.toUtf8)
    got == Ok ([("foo", "bar=baz" |> Str.toUtf8)])

split_list_U8 : List U8, U8 -> List (List U8)
split_list_U8 = \list, char ->
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

expect split_list_U8 [] 'a' == [[]]
expect split_list_U8 ['a', 'b', 'c'] 'b' == [['a'], ['c']]
expect split_list_U8 ['a', 'b', 'c'] 'c' == [['a', 'b'], []]
expect split_list_U8 ['a', 'b', 'c'] 'a' == [[], ['b', 'c']]
expect split_list_U8 ['a', 'b', 'b', 'c'] 'b' == [['a'], [], ['c']]
expect split_list_U8 ['a', 'b', 'c', 'b', 'd'] 'b' == [['a'], ['c'], ['d']]

url_decode : List U8 -> Result (List U8) [InvalidInput Str]
url_decode = \bytes ->
    bytes
    |> List.map
        \char -> if char == '+' then ' ' else char
    |> percentDecode

percentDecode : List U8 -> Result (List U8) [InvalidInput Str]
percentDecode = \bytes ->
    percent_decode_helper bytes (List.withCapacity (List.len bytes))

percent_decode_helper : List U8, List U8 -> Result (List U8) [InvalidInput Str]
percent_decode_helper = \bytes, result ->
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
                        percent_decode_helper (rest |> List.dropFirst 2) (result |> List.append num)

                    Err e ->
                        when e is
                            BadUtf8 _ _ | InvalidNumStr -> Err (InvalidInput "Can not decode percent value")
            else
                percent_decode_helper rest (result |> List.append first)

expect url_decode ("foo%20bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect url_decode ("foo+bar" |> Str.toUtf8) == Ok ("foo bar" |> Str.toUtf8)
expect url_decode ("foo%" |> Str.toUtf8) == Err (InvalidInput "Can not decode percent value")
expect url_decode ("foo%zz" |> Str.toUtf8) == Err (InvalidInput "Can not decode percent value")

get_field : List (Str, List U8), Str -> Result Str [InvalidInput Str, NotFound]
get_field = \fields, field_name ->
    fields
    |> List.findFirst \(element, _) -> element == field_name
    |> Result.try \(_, val) -> val |> Str.fromUtf8
    |> Result.mapErr
        \err ->
            when err is
                BadUtf8 _ _ -> InvalidInput "Can not decode value of $(field_name)"
                NotFound -> NotFound

expect get_field [("field", "value" |> Str.toUtf8)] "field" == Ok "value"
expect get_field [("field", "value" |> Str.toUtf8)] "other-field" == Err NotFound
