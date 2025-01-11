app [init_model, update_model, handle_request!, Model] {
    webserver: platform "https://github.com/ostcar/kingfisher/releases/download/v0.0.4/SHF-u_wznLGLRLdWiQ3XhuOwzrfXye9PhMGWmr36zzk.tar.br",
    html: "https://github.com/Hasnep/roc-html/releases/download/v0.6.0/IOyNfA4U_bCVBihrs95US9Tf5PGAWh3qvrBN4DRbK5c.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.11.0/z45Wzc-J39TLNweQUoLw3IGZtkQiEN3lTBv3BXErRjQ.tar.br",
}

import webserver.Http exposing [Request, Response]
import webserver.Host exposing [get!]
import html.Attribute exposing [attribute, class, id, name, rows, src, style, type, value]
import html.Html exposing [Node, button, div, form, h1, h2, img, input, p, renderWithoutDocType, section, span, text, textarea]
import json.Json
import "index.html" as index : Str
import "assets/styles.css" as styles : List U8
import "assets/htmx/htmx.min.js" as htmx : List U8
import "assets/spinner.svg" as spinner : List U8

Model : Dict ServiceId Service

ServiceId : Str

Service : {
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

SaveEvent : List U8 => {}

init_model : Model
init_model =
    Dict.empty {}

update_model : Model, List (List U8) -> Result Model [InvalidEvent Str]
update_model = \model, events ->
    events |> List.walkTry model \state, bytes -> state |> apply_event bytes

handle_request! : Request, Model => Result Response [InvalidEvent Str]
handle_request! = \request, model ->
    when request.method is
        Get ->
            handle_read_request request model |> Ok

        Post save_event! ->
            handle_write_request! request model save_event!

        _ ->
            response_400 "Method not allowed" |> Ok

# Events

Event : [UpdateService CalendarObject, UpdateInfo ServiceId Info Str]

Info : [Assistant, Reader, Notes]

event_to_bytes : Event -> List U8
event_to_bytes = \event ->
    when event is
        UpdateService calendar_object ->
            Encode.toBytes { name: "update_service", data: calendar_object } Json.utf8

        UpdateInfo service_id info info_text ->
            data = { service_id, info: info_to_string info, info_text }
            Encode.toBytes { name: "update_info", data } Json.utf8

event_from_bytes : List U8 -> Result Event [InvalidEvent Str]
event_from_bytes = \bytes ->
    Decode.fromBytes bytes Json.utf8
    |> Result.mapErr? \_ -> InvalidEvent "Cannot decode event name"
    |> \{ name: event_name } ->
        when event_name is
            "update_service" ->
                bytes
                |> Decode.fromBytes Json.utf8
                |> Result.mapErr? \_ -> InvalidEvent "Cannot decode event data"
                |> \{ data } -> UpdateService data
                |> Ok

            "update_info" ->
                bytes
                |> Decode.fromBytes Json.utf8
                |> Result.mapErr? \_ -> InvalidEvent "Cannot decode event data"
                |> \{ data } ->
                    info = info_from_string data.info |> Result.mapErr? \_ -> InvalidEvent "Cannot parse info from string"
                    UpdateInfo data.service_id info data.info_text |> Ok

            _ -> Err (InvalidEvent "Invalid event name")

apply_event : Model, List U8 -> Result Model [InvalidEvent Str]
apply_event = \model, encoded ->
    when encoded |> event_from_bytes? is
        UpdateService calendar_object ->
            model
            |> Dict.update calendar_object.veranstaltung.id \possible_value ->
                when possible_value is
                    Err Missing -> toService calendar_object "" "" "" |> Ok
                    Ok service -> toService calendar_object service.assistant service.reader service.notes |> Ok
            |> Ok

        UpdateInfo service_id info info_text ->
            model
            |> Dict.update service_id \possible_value ->
                possible_value
                |> Result.map \service ->
                    when info is
                        Assistant -> { service & assistant: info_text }
                        Reader -> { service & reader: info_text }
                        Notes -> { service & notes: info_text }
            |> Ok

info_to_string : Info -> Str
info_to_string = \s ->
    when s is
        Assistant -> "assistant"
        Reader -> "reader"
        Notes -> "notes"

info_from_string : Str -> Result Info [NotFound]
info_from_string = \s ->
    when s is
        "assistant" -> Ok Assistant
        "reader" -> Ok Reader
        "notes" -> Ok Notes
        _ -> Err NotFound

# Request handling

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
                service = model |> Dict.get service_id
                when (info_from_string info, service) is
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

        ["spinner.svg"] ->
            { body: spinner, headers: [{ name: "Content-Type", value: "image/svg+xml" }], status: 200 }

        _ ->
            { body: "404 Not Found" |> Str.toUtf8, headers: [], status: 404 }

handle_write_request! : Request, Model, SaveEvent => Result Response [InvalidEvent Str]
handle_write_request! = \request, model, save_event! ->
    result =
        when request.url |> Str.splitOn "/" is
            ["", "update-services"] ->
                update_services! request.body

            ["", "info-form", info, service_id] ->
                info_from_string info
                |> Result.try \inf ->
                    update_info_perform inf service_id request.body

            _ ->
                Err NotFound

    when result is
        Ok events ->
            bytes_list = events |> List.map event_to_bytes
            save_and_update! model bytes_list save_event!
            |> Result.map \updated_model ->
                updated_model |> list_view |> response_200

        Err NotFound -> response_400 "$(request.url) not found" |> Ok
        Err (BadRequest msg) -> response_400 msg |> Ok
        Err (InvalidCalendarData msg) -> response_500 msg |> Ok
        Err (HttpError msg) -> response_500 msg |> Ok

save_and_update! : Model, List (List U8), SaveEvent => Result Model [InvalidEvent Str]
save_and_update! = \model, events, save_event! ->
    List.forEach! events save_event!
    update_model model events

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
                |> List.concat (model |> Dict.toList |> List.map service_line)
                |> List.append update_button
            )
    renderWithoutDocType node

service_line : (ServiceId, Service) -> Node
service_line = \(service_id, service) ->
    section [] [
        h2 [] [text "$(date_to_str service.datetime) · $(time_to_str service.datetime) · $(service.location)"],
        p [] [text service.description],
        p [] [text "Leitung: $(service.pastor)"],
        div [class "person-line"] [
            p [class "info"] [text "Kirchner/in: ", button_or_text Assistant service_id service.assistant],
            p [class "info"] [text "Lektor/in: ", button_or_text Reader service_id service.reader],
        ],
        p [class "info"] [text "Bemerkungen: ", button_or_text Notes service_id service.notes],
    ]

button_or_text : Info, ServiceId, Str -> Node
button_or_text = \info, service_id, info_text ->
    if info_text == "" then
        button
            [
                (attribute "hx-get") "/info-form/$(info_to_string info)/$(service_id)",
                (attribute "hx-swap") "outerHTML",
            ]
            [text "Noch frei"]
    else
        span [] [
            span [style "margin-right:0.5em;"] [text info_text],
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

update_button : Node
update_button =
    section [id "update-services"] [
        button
            [(attribute "hx-post") "/update-services", (attribute "hx-target") "#mainContent", (attribute "hx-indicator") "#update-services"]
            [text "Termine aktualisieren"],
        img [class "htmx-indicator", src "/assets/spinner.svg"],
    ]

# Update services

update_services! : List U8 => Result (List Event) [InvalidCalendarData Str, HttpError Str]
update_services! = \_body ->
    when get! "https://kalender.evlks.de/json?vid=98&eventtype=1" is
        Err err ->
            Err (HttpError "Fetch external calendar: $(err)")

        Ok resp ->
            resp
            |> parse_calendar_entries?
            |> List.map \entry -> UpdateService entry
            |> Ok

toService : CalendarObject, Str, Str, Str -> Service
toService = \entry, assistant, reader, notes -> {
    datetime: transform_datetime entry.veranstaltung.start,
    location: entry.veranstaltung.place,
    description: "$(entry.veranstaltung.title): $(entry.veranstaltung.subtitle)",
    pastor: entry.veranstaltung.pastor,
    assistant: assistant,
    reader: reader,
    notes: notes,
}

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

parse_calendar_entries : List U8 -> Result (List CalendarObject) [InvalidCalendarData Str]
parse_calendar_entries = \data ->
    Decode.fromBytes data (Json.utf8With { fieldNameMapping: Custom calendar_field_name_mapping })
    |> Result.mapErr \err ->
        when err is
            Leftover bytes -> InvalidCalendarData "Can not parse calendar data: left over: $(bytes |> Str.fromUtf8 |> Result.withDefault "invalid")"
            TooShort -> InvalidCalendarData "Can not parse calendar data: too short"

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

expect
    got = transform_datetime "2013-07-20T14:00:00.000+02:00"
    expected = { year: 2013, month: 7, day: 20, hour: 14, minute: 0 }
    got == expected

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

update_info_perform : Info, ServiceId, List U8 -> Result (List Event) [BadRequest Str]
update_info_perform = \info, service_id, body ->
    body_to_fields body
    |> Result.try parse_info
    |> Result.mapErr? \err ->
        when err is
            InvalidInput msg -> BadRequest msg
    |> \info_text -> Ok [UpdateInfo service_id info info_text]

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
    { body: "Bad Request: $(msg)" |> Str.toUtf8, headers: [], status: 400 }

response_404 : Response
response_404 =
    { body: "Not Found" |> Str.toUtf8, headers: [], status: 404 }

response_500 : Str -> Response
response_500 = \msg ->
    { body: "Internal Server Error: $(msg)" |> Str.toUtf8, headers: [], status: 500 }

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
