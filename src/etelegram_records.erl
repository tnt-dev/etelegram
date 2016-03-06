-module(etelegram_records).
-compile({parse_transform, exprecs}).

-include("etelegram.hrl").

-export_records([etelegram_user,
                 etelegram_chat,
                 etelegram_photo_size,
                 etelegram_audio,
                 etelegram_document,
                 etelegram_sticker,
                 etelegram_video,
                 etelegram_voice,
                 etelegram_contact,
                 etelegram_location,
                 etelegram_message,
                 etelegram_user_profile_photos,
                 etelegram_file,
                 etelegram_reply_keyboard_markup,
                 etelegram_reply_keyboard_hide,
                 etelegram_force_reply,
                 etelegram_inline_query,
                 etelegram_inline_query_result_article,
                 etelegram_inline_query_result_photo,
                 etelegram_inline_query_result_gif,
                 etelegram_inline_query_result_mpeg4_gif,
                 etelegram_inline_query_result_video,
                 etelegram_chosen_inline_result,
                 etelegram_update,
                 etelegram_error]).

-export([parse/2, to_json/1]).

parse(_Type, undefined) -> undefined;
parse(Type, Props) when is_list(Props) ->
    improve('#fromlist-'(Props, '#new-'(Type))).

to_json(Rec) when is_tuple(Rec) ->
    [{K, to_json(V)} || {K, V} <- to_list(Rec), V =/= undefined];
to_json(V) -> V.

to_list(Rec) ->
    [Type|Fields] = tuple_to_list(Rec),
    lists:zip('#info-'(Type), Fields).

improve(#etelegram_document{thumb=Thumb}=Rec) ->
    Rec#etelegram_document{thumb=parse(etelegram_photo_size, Thumb)};
improve(#etelegram_sticker{thumb=Thumb}=Rec) ->
    Rec#etelegram_sticker{thumb=parse(etelegram_photo_size, Thumb)};
improve(#etelegram_video{thumb=Thumb}=Rec) ->
    Rec#etelegram_video{thumb=parse(etelegram_photo_size, Thumb)};
improve(#etelegram_user_profile_photos{photos=Photos}=Rec) ->
    Rec#etelegram_user_profile_photos{
      photos=parse(etelegram_photo_size, Photos)};
improve(#etelegram_message{from=From,
                           chat=Chat,
                           forward_from=ForwardFrom,
                           reply_to_message=ReplyToMessage,
                           audio=Audio,
                           document=Document,
                           photo=Photo,
                           sticker=Sticker,
                           video=Video,
                           voice=Voice,
                           contact=Contact,
                           location=Location,
                           new_chat_participant=NewChatParticipant,
                           left_chat_participant=LeftChatParticipant,
                           new_chat_photo=NewChatPhoto}=Rec) ->
    Rec#etelegram_message{
      from=parse(etelegram_user, From),
      chat=parse(etelegram_chat, Chat),
      forward_from=parse(etelegram_user, ForwardFrom),
      reply_to_message=parse(etelegram_message, ReplyToMessage),
      audio=parse(etelegram_audio, Audio),
      document=parse(etelegram_document, Document),
      photo=parse(etelegram_photo_size, Photo),
      sticker=parse(etelegram_sticker, Sticker),
      video=parse(etelegram_video, Video),
      voice=parse(etelegram_voice, Voice),
      contact=parse(etelegram_contact, Contact),
      location=parse(etelegram_location, Location),
      new_chat_participant=parse(etelegram_user, NewChatParticipant),
      left_chat_participant=parse(etelegram_user, LeftChatParticipant),
      new_chat_photo=parse(etelegram_photo_size, NewChatPhoto)};
improve(#etelegram_inline_query{from=From}=Rec) ->
    Rec#etelegram_inline_query{from=parse(etelegram_user, From)};
improve(#etelegram_update{message=Message,
                          inline_query=InlineQuery,
                          chosen_inline_result=ChosenInlineResult}=Rec) ->
    Rec#etelegram_update{
      message=parse(etelegram_message, Message),
      inline_query=parse(etelegram_inline_query, InlineQuery),
      chosen_inline_result=parse(etelegram_chosen_inline_result,
                                 ChosenInlineResult)};
improve(Rec) ->
    Rec.
