-record(etelegram_user, {id         :: pos_integer(),
                         first_name :: binary(),
                         last_name  :: binary(),
                         username   :: binary()}).

-record(etelegram_chat, {id         :: pos_integer(),
                         type       :: binary(),
                         title      :: binary(),
                         username   :: binary(),
                         first_name :: binary(),
                         last_name  :: binary()}).

-record(etelegram_photo_size, {file_id   :: binary(),
                               width     :: non_neg_integer(),
                               height    :: non_neg_integer(),
                               file_size :: non_neg_integer()}).

-record(etelegram_audio, {file_id   :: binary(),
                          duration  :: non_neg_integer(),
                          performer :: binary(),
                          title     :: binary(),
                          mime_type :: binary(),
                          file_size :: non_neg_integer()}).

-record(etelegram_document, {file_id   :: binary(),
                             thumb     :: #etelegram_photo_size{},
                             file_name :: binary(),
                             mime_type :: binary(),
                             file_size :: non_neg_integer()}).

-record(etelegram_sticker, {file_id   :: binary(),
                            width     :: non_neg_integer(),
                            height    :: non_neg_integer(),
                            thumb     :: #etelegram_photo_size{},
                            file_size :: non_neg_integer()}).

-record(etelegram_video, {file_id   :: binary(),
                          width     :: non_neg_integer(),
                          height    :: non_neg_integer(),
                          duration  :: non_neg_integer(),
                          thumb     :: #etelegram_photo_size{},
                          mime_type :: binary(),
                          file_size :: non_neg_integer()}).

-record(etelegram_voice, {file_id   :: binary(),
                          duration  :: non_neg_integer(),
                          mime_type :: binary(),
                          file_size :: non_neg_integer()}).

-record(etelegram_contact, {phone_number :: binary(),
                            first_name   :: binary(),
                            last_name    :: binary(),
                            user_id      :: pos_integer()}).

-record(etelegram_location, {longitude :: float(),
                             latitude  :: float()}).

-record(etelegram_message, {message_id              :: pos_integer(),
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

-record(etelegram_user_profile_photos, {total_count :: non_neg_integer(),
                                        photos      :: [#etelegram_photo_size{}]}).

-record(etelegram_file, {file_id   :: binary(),
                         file_size :: non_neg_integer(),
                         file_path :: binary()}).

-record(etelegram_reply_keyboard_markup, {keyboard                :: [[binary()]],
                                          resize_keyboard=false   :: boolean(),
                                          one_time_keyboard=false :: boolean(),
                                          selective=false         :: boolean()}).

-record(etelegram_reply_keyboard_hide, {hide_keyboard :: true,
                                        selective     :: boolean()}).

-record(etelegram_force_reply, {force_reply :: true,
                                selective   :: boolean()}).

-record(etelegram_update, {update_id :: non_neg_integer(),
                           message   :: #etelegram_message{}}).

-record(etelegram_error, {error_code  :: pos_integer(),
                          description :: binary()}).
