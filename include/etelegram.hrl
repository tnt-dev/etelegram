-record(etelegram_user,
        {id         :: pos_integer(),
         first_name :: binary(),
         last_name  :: binary(),
         username   :: binary()}).

-record(etelegram_chat,
        {id         :: pos_integer(),
         type       :: binary(),
         title      :: binary(),
         username   :: binary(),
         first_name :: binary(),
         last_name  :: binary()}).

-record(etelegram_photo_size,
        {file_id   :: binary(),
         width     :: non_neg_integer(),
         height    :: non_neg_integer(),
         file_size :: non_neg_integer()}).

-record(etelegram_audio,
        {file_id   :: binary(),
         duration  :: non_neg_integer(),
         performer :: binary(),
         title     :: binary(),
         mime_type :: binary(),
         file_size :: non_neg_integer()}).

-record(etelegram_document,
        {file_id   :: binary(),
         thumb     :: #etelegram_photo_size{},
         file_name :: binary(),
         mime_type :: binary(),
         file_size :: non_neg_integer()}).

-record(etelegram_sticker,
        {file_id   :: binary(),
         width     :: non_neg_integer(),
         height    :: non_neg_integer(),
         thumb     :: #etelegram_photo_size{},
         file_size :: non_neg_integer()}).

-record(etelegram_video,
        {file_id   :: binary(),
         width     :: non_neg_integer(),
         height    :: non_neg_integer(),
         duration  :: non_neg_integer(),
         thumb     :: #etelegram_photo_size{},
         mime_type :: binary(),
         file_size :: non_neg_integer()}).

-record(etelegram_voice,
        {file_id   :: binary(),
         duration  :: non_neg_integer(),
         mime_type :: binary(),
         file_size :: non_neg_integer()}).

-record(etelegram_contact,
        {phone_number :: binary(),
         first_name   :: binary(),
         last_name    :: binary(),
         user_id      :: pos_integer()}).

-record(etelegram_location,
        {longitude :: float(),
         latitude  :: float()}).

-record(etelegram_message,
        {message_id              :: pos_integer(),
         from                    :: #etelegram_user{},
         date                    :: pos_integer(),
         chat                    :: #etelegram_chat{},
         forward_from            :: #etelegram_user{},
         forward_date            :: pos_integer(),
         reply_to_message        :: #etelegram_message{},
         text                    :: binary(),
         audio                   :: #etelegram_audio{},
         document                :: #etelegram_document{},
         photo                   :: [#etelegram_photo_size{}],
         sticker                 :: #etelegram_sticker{},
         video                   :: #etelegram_video{},
         voice                   :: #etelegram_voice{},
         caption                 :: binary(),
         contact                 :: #etelegram_contact{},
         location                :: #etelegram_location{},
         new_chat_participant    :: #etelegram_user{},
         left_chat_participant   :: #etelegram_user{},
         new_chat_title          :: binary(),
         new_chat_photo          :: [#etelegram_photo_size{}],
         delete_chat_photo       :: true | undefined,
         group_chat_created      :: true | undefined,
         supergroup_chat_created :: true | undefined,
         channel_chat_created    :: true | undefined,
         migrate_to_chat_id      :: pos_integer(),
         migrate_from_chat_id    :: pos_integer()}).

-record(etelegram_user_profile_photos,
        {total_count :: non_neg_integer(),
         photos      :: [#etelegram_photo_size{}]}).

-record(etelegram_file,
        {file_id   :: binary(),
         file_size :: non_neg_integer(),
         file_path :: binary()}).

-record(etelegram_reply_keyboard_markup,
        {keyboard                :: [[binary()]],
         resize_keyboard=false   :: boolean(),
         one_time_keyboard=false :: boolean(),
         selective=false         :: boolean()}).

-record(etelegram_reply_keyboard_hide,
        {hide_keyboard :: true,
         selective     :: boolean()}).

-record(etelegram_force_reply,
        {force_reply :: true,
         selective   :: boolean()}).

-record(etelegram_inline_query_result_article,
        {type                     :: binary(),
         id                       :: binary(),
         title                    :: binary(),
         message_text             :: binary(),
         parse_mode               :: binary(),
         disable_web_page_preview :: boolean(),
         url 	                  :: binary(),
         hide_url                 :: boolean(),
         description 	          :: binary(),
         thumb_url                :: binary(),
         thumb_width              :: non_neg_integer(),
         thumb_height             :: non_neg_integer()}).

-record(etelegram_inline_query_result_photo,
        {type                     :: binary(),
         id                       :: binary(),
         photo_url                :: binary(),
         photo_width              :: non_neg_integer(),
         photo_height             :: non_neg_integer(),
         thumb_url                :: binary(),
         title 	                  :: binary(),
         description              :: binary(),
         caption                  :: binary(),
         message_text             :: binary(),
         parse_mode 	          :: binary(),
         disable_web_page_preview :: boolean()}).

-record(etelegram_inline_query_result_gif,
        {type                     :: binary(),
         id                       :: binary(),
         gif_url                  :: binary(),
         gif_width                :: non_neg_integer(),
         gif_height               :: non_neg_integer(),
         thumb_url                :: binary(),
         title 	                  :: binary(),
         caption                  :: binary(),
         message_text             :: binary(),
         parse_mode 	          :: binary(),
         disable_web_page_preview :: boolean()}).

-record(etelegram_inline_query_result_mpeg4_gif,
        {type                     :: binary(),
         id                       :: binary(),
         mpeg4_url                :: binary(),
         mpeg4_width              :: non_neg_integer(),
         mpeg4_height             :: non_neg_integer(),
         thumb_url                :: binary(),
         title 	                  :: binary(),
         caption                  :: binary(),
         message_text             :: binary(),
         parse_mode 	          :: binary(),
         disable_web_page_preview :: boolean()}).

-record(etelegram_inline_query_result_video,
        {type                     :: binary(),
         id                       :: binary(),
         video_url                :: binary(),
         mime_type                :: binary(),
         message_text             :: binary(),
         parse_mode 	          :: binary(),
         disable_web_page_preview :: boolean(),
         video_width              :: non_neg_integer(),
         video_height             :: non_neg_integer(),
         video_duration           :: non_neg_integer(),
         thumb_url                :: binary(),
         title 	                  :: binary(),
         description              :: binary()}).

-type inline_query_result() :: #etelegram_inline_query_result_article{}
                             | #etelegram_inline_query_result_photo{}
                             | #etelegram_inline_query_result_gif{}
                             | #etelegram_inline_query_result_mpeg4_gif{}
                             | #etelegram_inline_query_result_video{}.

-record(etelegram_inline_query,
        {id     :: binary(),
         result :: inline_query_result(),
         from   :: #etelegram_user{},
         query  :: binary(),
         offset :: binary()}).

-record(etelegram_chosen_inline_result,
        {result_id :: binary(),
         from      :: #etelegram_user{},
         query     :: binary()}).

-record(etelegram_update,
        {update_id            :: non_neg_integer(),
         message              :: #etelegram_message{},
         inline_query         :: #etelegram_inline_query{},
         chosen_inline_result :: #etelegram_chosen_inline_result{}}).

-record(etelegram_error,
        {error_code  :: pos_integer(),
         description :: binary()}).
