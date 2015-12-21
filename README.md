# etelegram


An Erlang wrapper around the
[Telegram Bot API](https://core.telegram.org/bots/api).

## Installation


Download the sources from [repositary](https://github.com/tnt-dev/etelegram)
and run `make`.

Or add it to `rebar.config` into a `deps` list

```erlang
{etelegram, ".*",
 {git, "git://github.com/tnt-dev/etelegram.git", {branch, "master"}}}
```

## Configuration

Add in your config

```erlang
{etelegram, [{token, <<"YOUR_BOT_TOKEN">>}]}
```

or configure it using `application:set_env/3`:

```erlang
1> application:load(etelegram).
2> application:set_env(etelegram, token, <<"YOUR_BOT_TOKEN">>).
```

## Usage

Add `etelegram` to the `application` property in your `appname.app.src` or
start it manually:

```erlang
3> etelegram:start().
```

## Examples

```erlang
4> etelegram:get_me().
{ok,#etelegram_user{id = 1010,
                    first_name = <<"Foo">>,last_name = undefined,
                    username = <<"foobot">>}}
```

```erlang
5> etelegram:send_message(1001, <<"Hello!">>).
{ok,#etelegram_message{message_id = 1,
                       from = #etelegram_user{id = 1010,
                                              first_name = <<"Foo">>,last_name = undefined,
                                              username = <<"foobot">>},
                       date = 1450640539,
                       chat = #etelegram_chat{id = 1001,type = <<"private">>,
                                              title = undefined,username = <<"bar">>,
                                              first_name = <<"Bar">>, last_name = undefined},
                       forward_from = undefined,forward_date = undefined,
                       reply_to_message = undefined,text = <<"Hello!">>,
                       audio = undefined,document = undefined,photo = undefined,
                       sticker = undefined,video = undefined,voice = undefined,
                       caption = undefined,contact = undefined,
                       location = undefined,new_chat_participant = undefined,
                       left_chat_participant = undefined,
                       new_chat_title = undefined,new_chat_photo = undefined,
                       delete_chat_photo = undefined,
                       group_chat_created = undefined}}
```
