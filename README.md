# telegram-bot

Haskell library/app for easy interacting with telegram

# install

`git clone https://github.com/robinvd/telegram-bot`

optional: edit the Main.hs with your token/extra extensions

```
stack build
stack exec telegram-exe
```

the default Main.hs expects your token as argument so:
`stack exec telegram-exe <token>`

# default modules

- help: responds with a hardcoded message

- timetable: fetches the timetable from the rug website, filters today, filters selected groups and responds with that info

# Configuring

to start the bot call the function:

`telegram :: TelegramSettings -> IO()`

from the module Telegram. Possible settings are: token, updateDelay, actions, defaultMsg

# making extensions:

type signature for extension should be `extension-name :: Action` `Action` is defined as:

`type Action = ((Int, [T.Text]) -> IO SendMessageRequest)`

as example you can take the help extension:

```
helpInfo :: Action
helpInfo (chatId, _) = return $ sendMessageRequest (T.pack $ show chatId) msg
  where msg = "Simple bot made by Robin to show timetables from the rug. available commands: /help, /timetable"
```

The second arguments is a list of arguments recieved, here it is ignored as the message is hardcoded

So `"/newfunction arg1 arg2"` will be parsed as `["/newfunction", "arg1", arg2"]`
