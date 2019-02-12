module MiniWorldWar.View.Error exposing (log)

import Http exposing (Error)

log : Error -> a -> a
log =
  Debug.log "error" >> always identity