-record(chat, {
          user,
          timestamp,
          message
         }).

-record(game, {
          id,
          turns = [],
          chat = [],
          players = [],
          victor = none,
          completed = false,
          logic_pg,
          state
         }).
