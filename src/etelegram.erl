-module(etelegram).
-behaviour(application).

-include("etelegram.hrl").

-define(APPS, [inets, crypto, asn1,  public_key, ssl,
               idna, mimerl, certifi, hackney]).

%% API
-export([start/0, stop/0]).

-export([get_me/0,
         send_message/2, send_message/3,
         forward_message/3,
         send_photo/2, send_photo/3,
         send_audio/2, send_audio/3,
         send_document/2, send_document/3,
         send_sticker/2, send_sticker/3,
         send_video/2, send_video/3,
         send_voice/2, send_voice/3,
         send_location/3, send_location/4,
         send_chat_action/2,
         get_user_profile_photos/1, get_user_profile_photos/2,
         get_updates/0, get_updates/1,
         set_webhook/1,
         get_file/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% Types

-type chat_id()             :: pos_integer() | binary().
-type message_id()          :: pos_integer().
-type user()                :: #etelegram_user{}.
-type message()             :: #etelegram_message{}.
-type file()                :: #etelegram_file{}.
-type user_profile_photos() :: #etelegram_user_profile_photos{}.
-type update()              :: #etelegram_update{}.
-type reply_markup()        :: #etelegram_reply_keyboard_markup{}
                             | #etelegram_reply_keyboard_hide{}
                             | #etelegram_force_reply{}.
-type error()               :: {error, #etelegram_error{}
                                     | {bad_response, binary()}
                                     | {connection_error, inet:posix()}}.

%% API

start() ->
    [etelegram_utils:ensure_started(App) || App <- ?APPS],
    ok = application:start(etelegram).

stop() ->
    ok = application:stop(etelegram).

%% @doc A simple method for testing your bot's auth token.
%% @end
-spec get_me() -> {ok, User} | Error when
      User  :: user(),
      Error :: error().
get_me() ->
    request(<<"getMe">>, etelegram_user).

%% @equiv send_message(ChatId, Text, [])
send_message(ChatId, Text) ->
    send_message(ChatId, Text, []).

%% @doc Use this method to send text messages.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Text': Text of the message to be sent
%%
%% Options:
%%
%% - `{parse_mode, binary()}': Send Markdown, if you want Telegram apps to
%% show bold, italic and inline URLs in your bot's message.
%% For the moment, only Telegram for Android supports this.
%% - `{disable_web_page_preview, bool()}': Disables link previews for links
%% in this message
%% - `{reply_to_message_id, pos_integer()}': If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_message(ChatId, Text, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Text    :: binary(),
      Opts    :: [{atom(), binary()
                         | message_id()
                         | boolean()
                         | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_message(ChatId, Text, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {text, Text} | Opts],
    request(<<"sendMessage">>, etelegram_message, Opts2).

%% @doc Use this method to forward messages of any kind.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `FromChatId': Unique identifier for the chat where the original message
%% was sent (or channel username in the format @channelusername)
%% - `MessageId': Unique message identifier
%% @end
-spec forward_message(ChatId, FromChatId, MessageId)
     -> {ok, Message} | Error when
      ChatId     :: chat_id(),
      FromChatId :: chat_id(),
      MessageId  :: message_id(),
      Message    :: message(),
      Error      :: error().
forward_message(ChatId, FromChatId, MessageId) ->
    Opts = [{chat_id, ChatId},
            {from_chat_id, FromChatId},
            {message_id, MessageId}],
    request(<<"forwardMessage">>, etelegram_message, Opts).

%% @equiv send_photo(ChatId, Photo, [])
send_photo(ChatId, Photo) ->
    send_photo(ChatId, Photo, []).

%% @doc Use this method to send photos.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Photo': Photo to send. You can either pass a file_id as String to
%% resend a photo that is already on the Telegram servers, or upload
%% a new photo using multipart/form-data.
%%
%% Options:
%%
%% - `{caption, binary()}': Photo caption (may also be used when resending
%% photos by file_id)
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_photo(ChatId, Photo, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Photo   :: binary(),
      Opts    :: [{atom(), binary()
                         | message_id()
                         | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_photo(ChatId, Photo, Opts) ->
    Opts2 = [{chat_id, ChatId},
            {photo, Photo} | Opts],
    request(<<"sendPhoto">>, etelegram_message, Opts2).

%% @equiv send_audio(ChatId, Audio, [])
send_audio(ChatId, Audio) ->
    send_audio(ChatId, Audio, []).

%% @doc Use this method to send audio files, if you want Telegram clients
%% to display them in the music player.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Audio': Audio to send. You can either pass a file_id as String to
%% resend a audio that is already on the Telegram servers, or upload
%% a new audio using multipart/form-data.
%%
%% Options:
%%
%% - `{duration, pos_integer()}': Duration of the audio in seconds
%% - `{performer, binary()}': Performer
%% - `{title, binary()}': Track name
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_audio(ChatId, Audio, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Audio   :: binary(),
      Opts    :: [{atom(), pos_integer()
                         | binary()
                         | message_id()
                         | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_audio(ChatId, Audio, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {audio, Audio} | Opts],
    request(<<"sendAudio">>, etelegram_message, Opts2).

%% @equiv send_document(ChatId, Document, [])
send_document(ChatId, Document) ->
    send_document(ChatId, Document, []).

%% @doc Use this method to send general files.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Document': File to send. You can either pass a file_id as String to
%% resend a file that is already on the Telegram servers, or upload
%% a new file using multipart/form-data.
%%
%% Options:
%%
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% end
-spec send_document(ChatId, Document, Opts) -> {ok, Message} | Error when
      ChatId   :: chat_id(),
      Document :: binary(),
      Opts     :: [{atom(), message_id() | reply_markup()}],
      Message  :: message(),
      Error    :: error().
send_document(ChatId, Document, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {document, Document} | Opts],
    request(<<"sendDocument">>, etelegram_message, Opts2).

%% @equiv send_sticker(ChatId, Sticker, [])
send_sticker(ChatId, Sticker) ->
    send_sticker(ChatId, Sticker, []).

%% @doc Use this method to send .webp stickers.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Sticker': Sticker to send. You can either pass a file_id as String to
%% resend a sticker that is already on the Telegram servers, or upload
%% a new sticker using multipart/form-data.
%%
%% Options:
%%
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_sticker(ChatId, Sticker, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Sticker :: binary(),
      Opts    :: [{atom(), message_id() | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_sticker(ChatId, Sticker, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {sticker, Sticker} | Opts],
    request(<<"sendSticker">>, etelegram_message, Opts2).

%% equiv send_video(ChatId, Video, [])
send_video(ChatId, Video) ->
    send_audio(ChatId, Video, []).

%% @doc Use this method to send video files, Telegram clients support mp4 videos
%% (other formats may be sent as Document).
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Video': Video to send. You can either pass a file_id as String to
%% resend a video that is already on the Telegram servers, or upload
%% a new video file using multipart/form-data.
%% Options:
%%
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_video(ChatId, Video, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Video   :: binary(),
      Opts    :: [{atom(), message_id() | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_video(ChatId, Video, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {video, Video} | Opts],
    request(<<"sendVideo">>, etelegram_message, Opts2).

%% @equiv send_voice(ChatId, Voice, [])
send_voice(ChatId, Voice) ->
    send_audio(ChatId, Voice, []).

%% @doc Use this method to send audio files, if you want Telegram clients to
%% display the file as a playable voice message. For this to work, your audio
%% must be in an .ogg file encoded with OPUS (other formats may be sent
%% as Audio or Document).
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Voice': Audio file to send. You can either pass a file_id as String to
%% resend an audio that is already on the Telegram servers, or upload
%% a new audio file using multipart/form-data.
%%
%% Options:
%%
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_voice(ChatId, Voice, Opts) -> {ok, Message} | Error when
      ChatId  :: chat_id(),
      Voice   :: binary(),
      Opts    :: [{atom(), message_id() | reply_markup()}],
      Message :: message(),
      Error   :: error().
send_voice(ChatId, Voice, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {voice, Voice} | Opts],
    request(<<"sendVoice">>, etelegram_message, Opts2).

%% @equiv send_location(ChatId, Latitude, Longitude, [])
send_location(ChatId, Latitude, Longitude) ->
    send_location(ChatId, Latitude, Longitude, []).

%% @doc Use this method to send point on the map.
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Latitude': Latitude of location
%% - `Longitude': Longitude of location
%%
%% Options:
%%
%% - '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
%% the original message
%% - `{reply_markup, reply_markup()}': Additional interface options.
%% A JSON-serialized object for a custom reply keyboard,
%% instructions to hide keyboard or to force a reply from the user
%% @end
-spec send_location(ChatId, Latitude, Longitude, Opts)
    -> {ok, Message} | Error when
      ChatId    :: chat_id(),
      Latitude  :: float(),
      Longitude :: float(),
      Opts      :: [{atom(), message_id() | reply_markup()}],
      Message   :: message(),
      Error     :: error().
send_location(ChatId, Latitude, Longitude, Opts) ->
    Opts2 = [{chat_id, ChatId},
             {latitude, Latitude},
             {longitude, Longitude} | Opts],
    request(<<"sendLocation">>, etelegram_message, Opts2).

%% @doc Use this method when you need to tell the user that something is
%% happening on the bot's side. The status is set for 5 seconds or less
%% (when a message arrives from your bot, Telegram clients clear
%% its typing status).
%%
%% Args:
%%
%% - `ChatId': Unique identifier for the target chat or
%% username of the target channel (in the format @channelusername)
%% - `Action': Type of action to broadcast. Choose one, depending on what the
%% user is about to receive: `typing' for text messages, `upload_photo' for
%% photos, `record_video' or upload_video for videos, `record_audio' or
%% `upload_audio' for audio files, `upload_document' for general files,
%% find_location for location data.
%% end
-spec send_chat_action(ChatId, Action) -> ok when
      ChatId :: chat_id(),
      Action :: binary().
send_chat_action(ChatId, Action) ->
    Opts = [{chat_id, ChatId},
            {action, Action}],
    request(<<"sendChatAction">>, undefined, Opts).

%% @equiv get_user_profile_photos(ChatId, [])
get_user_profile_photos(ChatId) ->
    get_user_profile_photos(ChatId, []).

%% @doc Use this method to get a list of profile pictures for a user.
%%
%% Args:
%%
%% -`UserId': Unique identifier of the target user
%%
%% Options:
%%
%% - `{offset, pos_integer()}': Sequential number of the first photo to
%% be returned. By default, all photos are returned.
%% - `{limit, pos_integer()}': Limits the number of photos to be retrieved.
%% Values between 1—100 are accepted. Defaults to 100.
%% @end
-spec get_user_profile_photos(ChatId, Opts) -> {ok, Photos} | Error when
      ChatId :: chat_id(),
      Opts   :: [{atom(), pos_integer()}],
      Photos :: user_profile_photos(),
      Error  :: error().
get_user_profile_photos(UserId, Opts) ->
    Opts2 = [{user_id, UserId} | Opts],
    request(<<"getUserProfilePhotos">>, etelegram_user_profile_photos, Opts2).

%% @equiv get_updates([])
get_updates() ->
    get_updates([]).

%% @doc Use this method to receive incoming updates using long polling.
%%
%% Options:
%%
%% - `{offset, pos_integer()': Identifier of the first update to be returned.
%% Must be greater by one than the highest among the identifiers of previously
%% received updates. By default, updates starting with the earliest unconfirmed
%% update are returned. An update is considered confirmed as soon as
%% `getUpdates' is called with an offset higher than its `update_id'.
%% - `{limit, pos_integer()}': Limits the number of updates to be retrieved.
%% Values between 1—100 are accepted. Defaults to 100
%% - `{timeout, pos_integer()}': Timeout in seconds for long polling.
%% Defaults to 0, i.e. usual short polling
%% @end
-spec get_updates(Opts) -> {ok, Updates} | Error when
      Opts    :: [{atom(), pos_integer()}],
      Updates :: [update()],
      Error   :: error().
get_updates(Opts) ->
    request(<<"getUpdates">>, etelegram_update, Opts).

%% @doc Use this method to specify a url and receive incoming updates via
%% an outgoing webhook.
%%
%% Options:
%%
%% - `{url, binary()}': HTTPS url to send updates to. Use an empty string
%% to remove webhook integration
%% - `{certificate, binary()}': Upload your public key certificate so that the
%% root certificate in use can be checked
%% @end
-spec set_webhook(Opts) -> ok when
      Opts :: [{atom(), binary()}].
set_webhook(Opts) ->
    request(<<"setWebhook">>, undefined, Opts).

%% @doc Use this method to get basic info about a file and prepare it
%% for downloading.
%%
%% Args:
%%
%% - `FileId': File identifier to get info about
%% @end
-spec get_file(FileId) -> {ok, File} | Error when
      FileId :: binary(),
      File   :: file(),
      Error  :: error().
get_file(FileId) ->
    Opts = [{file_id, FileId}],
    request(<<"getFile">>, etelegram_file, Opts).


request(Method, RecType) ->
    request(Method, RecType, []).

request(Method, RecType, Opts) ->
    etelegram_http:request(Method, RecType, Opts).

%% Application callbacks

start(_StartType, _StartArgs) ->
    etelegram_sup:start_link().

stop(_State) ->
    ok.
