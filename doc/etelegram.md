

# Module etelegram #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-chat_id">chat_id()</a> ###


<pre><code>
chat_id() = pos_integer() | binary()
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error,#etelegram_error{error_code = undefined | pos_integer(),description = undefined | binary()} |{bad_response, binary()} |{connection_error, <a href="inet.md#type-posix">inet:posix()</a>}}
</code></pre>




### <a name="type-file">file()</a> ###


<pre><code>
file() = #etelegram_file{file_id = undefined | binary(),file_size = undefined | non_neg_integer(),file_path = undefined | binary()}
</code></pre>




### <a name="type-message">message()</a> ###


<pre><code>
message() = #etelegram_message{message_id = undefined | pos_integer(),from = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},date = undefined | pos_integer(),chat = undefined| #etelegram_chat{id = undefined| pos_integer(),type = undefined| binary(),title = undefined| binary(),username = undefined| binary(),first_name = undefined| binary(),last_name = undefined| binary()},forward_from = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},forward_date = undefined | pos_integer(),reply_to_message = undefined| #etelegram_message{message_id = undefined| pos_integer(),from = undefined| #etelegram_user{},date = undefined| pos_integer(),chat = undefined| #etelegram_chat{},forward_from = undefined| #etelegram_user{},forward_date = undefined| pos_integer(),reply_to_message = undefined| #etelegram_message{},text = undefined| binary(),audio = undefined| #etelegram_audio{},document = undefined| #etelegram_document{},photo = undefined| [#etelegram_photo_size{}],sticker = undefined| #etelegram_sticker{},video = undefined| #etelegram_video{},voice = undefined| #etelegram_voice{},caption = undefined| binary(),contact = undefined| #etelegram_contact{},location = undefined| #etelegram_location{},new_chat_participant = undefined| #etelegram_user{},left_chat_participant = undefined| #etelegram_user{},new_chat_title = undefined| binary(),new_chat_photo = undefined| [#etelegram_photo_size{}],delete_chat_photo = true| undefined,group_chat_created = true| undefined},text = undefined | binary(),audio = undefined| #etelegram_audio{file_id = undefined| binary(),duration = undefined| non_neg_integer(),performer = undefined| binary(),title = undefined| binary(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},document = undefined| #etelegram_document{file_id = undefined| binary(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},file_name = undefined| binary(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},photo = undefined| [#etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()}],sticker = undefined| #etelegram_sticker{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},file_size = undefined| non_neg_integer()},video = undefined| #etelegram_video{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),duration = undefined| non_neg_integer(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},voice = undefined| #etelegram_voice{file_id = undefined| binary(),duration = undefined| non_neg_integer(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},caption = undefined | binary(),contact = undefined| #etelegram_contact{phone_number = undefined| binary(),first_name = undefined| binary(),last_name = undefined| binary(),user_id = undefined| pos_integer()},location = undefined| #etelegram_location{longitude = undefined| float(),latitude = undefined| float()},new_chat_participant = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},left_chat_participant = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},new_chat_title = undefined | binary(),new_chat_photo = undefined| [#etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()}],delete_chat_photo = true | undefined,group_chat_created = true | undefined}
</code></pre>




### <a name="type-message_id">message_id()</a> ###


<pre><code>
message_id() = pos_integer()
</code></pre>




### <a name="type-reply_markup">reply_markup()</a> ###


<pre><code>
reply_markup() = #etelegram_reply_keyboard_markup{keyboard = undefined| [[binary()]],resize_keyboard =boolean(),one_time_keyboard =boolean(),selective =boolean()}| #etelegram_reply_keyboard_hide{hide_keyboard = undefined| true,selective = undefined| boolean()}| #etelegram_force_reply{force_reply = undefined| true,selective = undefined| boolean()}
</code></pre>




### <a name="type-update">update()</a> ###


<pre><code>
update() = #etelegram_update{update_id = undefined | non_neg_integer(),message = undefined| #etelegram_message{message_id = undefined| pos_integer(),from = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},date = undefined| pos_integer(),chat = undefined| #etelegram_chat{id = undefined| pos_integer(),type = undefined| binary(),title = undefined| binary(),username = undefined| binary(),first_name = undefined| binary(),last_name = undefined| binary()},forward_from = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},forward_date = undefined| pos_integer(),reply_to_message = undefined| #etelegram_message{message_id = undefined| pos_integer(),from = undefined| #etelegram_user{},date = undefined| pos_integer(),chat = undefined| #etelegram_chat{},forward_from = undefined| #etelegram_user{},forward_date = undefined| pos_integer(),reply_to_message = undefined| #etelegram_message{},text = undefined| binary(),audio = undefined| #etelegram_audio{},document = undefined| #etelegram_document{},photo = undefined| [#etelegram_photo_size{}],sticker = undefined| #etelegram_sticker{},video = undefined| #etelegram_video{},voice = undefined| #etelegram_voice{},caption = undefined| binary(),contact = undefined| #etelegram_contact{},location = undefined| #etelegram_location{},new_chat_participant = undefined| #etelegram_user{},left_chat_participant = undefined| #etelegram_user{},new_chat_title = undefined| binary(),new_chat_photo = undefined| [#etelegram_photo_size{}],delete_chat_photo = true| undefined,group_chat_created = true| undefined},text = undefined| binary(),audio = undefined| #etelegram_audio{file_id = undefined| binary(),duration = undefined| non_neg_integer(),performer = undefined| binary(),title = undefined| binary(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},document = undefined| #etelegram_document{file_id = undefined| binary(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},file_name = undefined| binary(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},photo = undefined| [#etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()}],sticker = undefined| #etelegram_sticker{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},file_size = undefined| non_neg_integer()},video = undefined| #etelegram_video{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),duration = undefined| non_neg_integer(),thumb = undefined| #etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()},mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},voice = undefined| #etelegram_voice{file_id = undefined| binary(),duration = undefined| non_neg_integer(),mime_type = undefined| binary(),file_size = undefined| non_neg_integer()},caption = undefined| binary(),contact = undefined| #etelegram_contact{phone_number = undefined| binary(),first_name = undefined| binary(),last_name = undefined| binary(),user_id = undefined| pos_integer()},location = undefined| #etelegram_location{longitude = undefined| float(),latitude = undefined| float()},new_chat_participant = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},left_chat_participant = undefined| #etelegram_user{id = undefined| pos_integer(),first_name = undefined| binary(),last_name = undefined| binary(),username = undefined| binary()},new_chat_title = undefined| binary(),new_chat_photo = undefined| [#etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()}],delete_chat_photo = true| undefined,group_chat_created = true| undefined}}
</code></pre>




### <a name="type-user">user()</a> ###


<pre><code>
user() = #etelegram_user{id = undefined | pos_integer(),first_name = undefined | binary(),last_name = undefined | binary(),username = undefined | binary()}
</code></pre>




### <a name="type-user_profile_photos">user_profile_photos()</a> ###


<pre><code>
user_profile_photos() = #etelegram_user_profile_photos{total_count = undefined| non_neg_integer(),photos = undefined| [#etelegram_photo_size{file_id = undefined| binary(),width = undefined| non_neg_integer(),height = undefined| non_neg_integer(),file_size = undefined| non_neg_integer()}]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#forward_message-3">forward_message/3</a></td><td>Use this method to forward messages of any kind.</td></tr><tr><td valign="top"><a href="#get_file-1">get_file/1</a></td><td>Use this method to get basic info about a file and prepare it
for downloading.</td></tr><tr><td valign="top"><a href="#get_me-0">get_me/0</a></td><td>A simple method for testing your bot's auth token.</td></tr><tr><td valign="top"><a href="#get_updates-0">get_updates/0</a></td><td>Equivalent to <a href="#get_updates-1"><tt>get_updates([])</tt></a>.</td></tr><tr><td valign="top"><a href="#get_updates-1">get_updates/1</a></td><td>Use this method to receive incoming updates using long polling.</td></tr><tr><td valign="top"><a href="#get_user_profile_photos-1">get_user_profile_photos/1</a></td><td>Equivalent to <a href="#get_user_profile_photos-2"><tt>get_user_profile_photos(ChatId, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#get_user_profile_photos-2">get_user_profile_photos/2</a></td><td>Use this method to get a list of profile pictures for a user.</td></tr><tr><td valign="top"><a href="#send_audio-2">send_audio/2</a></td><td>Equivalent to <a href="#send_audio-3"><tt>send_audio(ChatId, Audio, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_audio-3">send_audio/3</a></td><td>Use this method to send audio files, if you want Telegram clients
to display them in the music player.</td></tr><tr><td valign="top"><a href="#send_chat_action-2">send_chat_action/2</a></td><td>Use this method when you need to tell the user that something is
happening on the bot's side.</td></tr><tr><td valign="top"><a href="#send_document-2">send_document/2</a></td><td>Equivalent to <a href="#send_document-3"><tt>send_document(ChatId, Document, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_document-3">send_document/3</a></td><td>Use this method to send general files.</td></tr><tr><td valign="top"><a href="#send_location-3">send_location/3</a></td><td>Equivalent to <a href="#send_location-4"><tt>send_location(ChatId, Latitude, Longitude, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_location-4">send_location/4</a></td><td>Use this method to send point on the map.</td></tr><tr><td valign="top"><a href="#send_message-2">send_message/2</a></td><td>Equivalent to <a href="#send_message-3"><tt>send_message(ChatId, Text, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_message-3">send_message/3</a></td><td>Use this method to send text messages.</td></tr><tr><td valign="top"><a href="#send_photo-2">send_photo/2</a></td><td>Equivalent to <a href="#send_photo-3"><tt>send_photo(ChatId, Photo, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_photo-3">send_photo/3</a></td><td>Use this method to send photos.</td></tr><tr><td valign="top"><a href="#send_sticker-2">send_sticker/2</a></td><td>Equivalent to <a href="#send_sticker-3"><tt>send_sticker(ChatId, Sticker, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_sticker-3">send_sticker/3</a></td><td>Use this method to send .webp stickers.</td></tr><tr><td valign="top"><a href="#send_video-2">send_video/2</a></td><td></td></tr><tr><td valign="top"><a href="#send_video-3">send_video/3</a></td><td>Use this method to send video files, Telegram clients support mp4 videos
(other formats may be sent as Document).</td></tr><tr><td valign="top"><a href="#send_voice-2">send_voice/2</a></td><td>Equivalent to <a href="#send_voice-3"><tt>send_voice(ChatId, Voice, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#send_voice-3">send_voice/3</a></td><td>Use this method to send audio files, if you want Telegram clients to
display the file as a playable voice message.</td></tr><tr><td valign="top"><a href="#set_webhook-1">set_webhook/1</a></td><td>Use this method to specify a url and receive incoming updates via
an outgoing webhook.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

<a name="get_me-0"></a>

### get_me/0 ###

<pre><code>
get_me() -&gt; {ok, User} | Error
</code></pre>

<ul class="definitions"><li><code>User = <a href="#type-user">user()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

A simple method for testing your bot's auth token.

<a name="send_message-2"></a>

### send_message/2 ###

`send_message(ChatId, Text) -> any()`

Equivalent to [`send_message(ChatId, Text, [])`](#send_message-3).

<a name="send_message-3"></a>

### send_message/3 ###

<pre><code>
send_message(ChatId, Text, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Text = binary()</code></li><li><code>Opts = [{atom(),binary() | <a href="#type-message_id">message_id()</a> | boolean() | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send text messages.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Text`: Text of the message to be sent

Options:

- `{parse_mode, binary()}`: Send Markdown, if you want Telegram apps to
show bold, italic and inline URLs in your bot's message.
For the moment, only Telegram for Android supports this.
- `{disable_web_page_preview, bool()}`: Disables link previews for links
in this message
- `{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="forward_message-3"></a>

### forward_message/3 ###

<pre><code>
forward_message(ChatId, FromChatId, MessageId) -&gt;{ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>FromChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>MessageId = <a href="#type-message_id">message_id()</a></code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to forward messages of any kind.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `FromChatId`: Unique identifier for the chat where the original message
was sent (or channel username in the format @channelusername)
- `MessageId`: Unique message identifier

<a name="send_photo-2"></a>

### send_photo/2 ###

`send_photo(ChatId, Photo) -> any()`

Equivalent to [`send_photo(ChatId, Photo, [])`](#send_photo-3).

<a name="send_photo-3"></a>

### send_photo/3 ###

<pre><code>
send_photo(ChatId, Photo, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Photo = binary()</code></li><li><code>Opts = [{atom(), binary() | <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send photos.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Photo`: Photo to send. You can either pass a file_id as String to
resend a photo that is already on the Telegram servers, or upload
a new photo using multipart/form-data.

Options:

- `{caption, binary()}`: Photo caption (may also be used when resending
photos by file_id)
- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_audio-2"></a>

### send_audio/2 ###

`send_audio(ChatId, Audio) -> any()`

Equivalent to [`send_audio(ChatId, Audio, [])`](#send_audio-3).

<a name="send_audio-3"></a>

### send_audio/3 ###

<pre><code>
send_audio(ChatId, Audio, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Audio = binary()</code></li><li><code>Opts = [{atom(),pos_integer() | binary() | <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send audio files, if you want Telegram clients
to display them in the music player.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Audio`: Audio to send. You can either pass a file_id as String to
resend a audio that is already on the Telegram servers, or upload
a new audio using multipart/form-data.

Options:

- `{duration, pos_integer()}`: Duration of the audio in seconds
- `{performer, binary()}`: Performer
- `{title, binary()}`: Track name
- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_document-2"></a>

### send_document/2 ###

`send_document(ChatId, Document) -> any()`

Equivalent to [`send_document(ChatId, Document, [])`](#send_document-3).

<a name="send_document-3"></a>

### send_document/3 ###

<pre><code>
send_document(ChatId, Document, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Document = binary()</code></li><li><code>Opts = [{atom(), <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send general files.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Document`: File to send. You can either pass a file_id as String to
resend a file that is already on the Telegram servers, or upload
a new file using multipart/form-data.

Options:

- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user
end

<a name="send_sticker-2"></a>

### send_sticker/2 ###

`send_sticker(ChatId, Sticker) -> any()`

Equivalent to [`send_sticker(ChatId, Sticker, [])`](#send_sticker-3).

<a name="send_sticker-3"></a>

### send_sticker/3 ###

<pre><code>
send_sticker(ChatId, Sticker, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Sticker = binary()</code></li><li><code>Opts = [{atom(), <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send .webp stickers.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Sticker`: Sticker to send. You can either pass a file_id as String to
resend a sticker that is already on the Telegram servers, or upload
a new sticker using multipart/form-data.

Options:

- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_video-2"></a>

### send_video/2 ###

`send_video(ChatId, Video) -> any()`

<a name="send_video-3"></a>

### send_video/3 ###

<pre><code>
send_video(ChatId, Video, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Video = binary()</code></li><li><code>Opts = [{atom(), <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send video files, Telegram clients support mp4 videos
(other formats may be sent as Document).

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Video`: Video to send. You can either pass a file_id as String to
resend a video that is already on the Telegram servers, or upload
a new video file using multipart/form-data.
Options:

- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_voice-2"></a>

### send_voice/2 ###

`send_voice(ChatId, Voice) -> any()`

Equivalent to [`send_voice(ChatId, Voice, [])`](#send_voice-3).

<a name="send_voice-3"></a>

### send_voice/3 ###

<pre><code>
send_voice(ChatId, Voice, Opts) -&gt; {ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Voice = binary()</code></li><li><code>Opts = [{atom(), <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send audio files, if you want Telegram clients to
display the file as a playable voice message. For this to work, your audio
must be in an .ogg file encoded with OPUS (other formats may be sent
as Audio or Document).

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Voice`: Audio file to send. You can either pass a file_id as String to
resend an audio that is already on the Telegram servers, or upload
a new audio file using multipart/form-data.

Options:

- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_location-3"></a>

### send_location/3 ###

`send_location(ChatId, Latitude, Longitude) -> any()`

Equivalent to [`send_location(ChatId, Latitude, Longitude, [])`](#send_location-4).

<a name="send_location-4"></a>

### send_location/4 ###

<pre><code>
send_location(ChatId, Latitude, Longitude, Opts) -&gt;{ok, Message} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Latitude = float()</code></li><li><code>Longitude = float()</code></li><li><code>Opts = [{atom(), <a href="#type-message_id">message_id()</a> | <a href="#type-reply_markup">reply_markup()</a>}]</code></li><li><code>Message = <a href="#type-message">message()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to send point on the map.

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Latitude`: Latitude of location
- `Longitude`: Longitude of location

Options:

- '{reply_to_message_id, pos_integer()}`: If the message is a reply, ID of
the original message
- `{reply_markup, reply_markup()}`: Additional interface options.
A JSON-serialized object for a custom reply keyboard,
instructions to hide keyboard or to force a reply from the user

<a name="send_chat_action-2"></a>

### send_chat_action/2 ###

<pre><code>
send_chat_action(ChatId, Action) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Action = binary()</code></li></ul>

Use this method when you need to tell the user that something is
happening on the bot's side. The status is set for 5 seconds or less
(when a message arrives from your bot, Telegram clients clear
its typing status).

Args:

- `ChatId`: Unique identifier for the target chat or
username of the target channel (in the format @channelusername)
- `Action`: Type of action to broadcast. Choose one, depending on what the
user is about to receive: `typing` for text messages, `upload_photo` for
photos, `record_video` or upload_video for videos, `record_audio` or
`upload_audio` for audio files, `upload_document` for general files,
find_location for location data.
end

<a name="get_user_profile_photos-1"></a>

### get_user_profile_photos/1 ###

`get_user_profile_photos(ChatId) -> any()`

Equivalent to [`get_user_profile_photos(ChatId, [])`](#get_user_profile_photos-2).

<a name="get_user_profile_photos-2"></a>

### get_user_profile_photos/2 ###

<pre><code>
get_user_profile_photos(ChatId, Opts) -&gt; {ok, Photos} | Error
</code></pre>

<ul class="definitions"><li><code>ChatId = <a href="#type-chat_id">chat_id()</a></code></li><li><code>Opts = [{atom(), pos_integer()}]</code></li><li><code>Photos = <a href="#type-user_profile_photos">user_profile_photos()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to get a list of profile pictures for a user.

Args:

-`UserId`: Unique identifier of the target user

Options:

- `{offset, pos_integer()}`: Sequential number of the first photo to
be returned. By default, all photos are returned.
- `{limit, pos_integer()}`: Limits the number of photos to be retrieved.
Values between 1—100 are accepted. Defaults to 100.

<a name="get_updates-0"></a>

### get_updates/0 ###

`get_updates() -> any()`

Equivalent to [`get_updates([])`](#get_updates-1).

<a name="get_updates-1"></a>

### get_updates/1 ###

<pre><code>
get_updates(Opts) -&gt; {ok, Updates} | Error
</code></pre>

<ul class="definitions"><li><code>Opts = [{atom(), pos_integer()}]</code></li><li><code>Updates = [<a href="#type-update">update()</a>]</code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to receive incoming updates using long polling.

Options:

- `{offset, pos_integer()`: Identifier of the first update to be returned.
Must be greater by one than the highest among the identifiers of previously
received updates. By default, updates starting with the earliest unconfirmed
update are returned. An update is considered confirmed as soon as
`getUpdates` is called with an offset higher than its `update_id`.
- `{limit, pos_integer()}`: Limits the number of updates to be retrieved.
Values between 1—100 are accepted. Defaults to 100
- `{timeout, pos_integer()}`: Timeout in seconds for long polling.
Defaults to 0, i.e. usual short polling

<a name="set_webhook-1"></a>

### set_webhook/1 ###

<pre><code>
set_webhook(Opts) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Opts = [{atom(), binary()}]</code></li></ul>

Use this method to specify a url and receive incoming updates via
an outgoing webhook.

Options:

- `{url, binary()}`: HTTPS url to send updates to. Use an empty string
to remove webhook integration
- `{certificate, binary()}`: Upload your public key certificate so that the
root certificate in use can be checked

<a name="get_file-1"></a>

### get_file/1 ###

<pre><code>
get_file(FileId) -&gt; {ok, File} | Error
</code></pre>

<ul class="definitions"><li><code>FileId = binary()</code></li><li><code>File = <a href="#type-file">file()</a></code></li><li><code>Error = <a href="#type-error">error()</a></code></li></ul>

Use this method to get basic info about a file and prepare it
for downloading.

Args:

- `FileId`: File identifier to get info about

<a name="start-2"></a>

### start/2 ###

`start(StartType, StartArgs) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(State) -> any()`

