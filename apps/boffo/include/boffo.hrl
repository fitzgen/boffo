-record(chat, {
          user,
          timestamp,
          message
         }).

-record(game, {
          id,
          %% Turns are ordered newest to oldest.
          turns = [],
          %% Chat messages are ordered newest to oldest.
          chat = [],
          players = sets:new(),
          victor = none,
          completed = false,
          logic_pg,
          state
         }).

-type uuid()          :: binary().
-type boffo_game_id() :: uuid().

%% -export_type([uuid/0, boffo_game_id/0]).
