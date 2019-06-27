module AsteroidMiner.Lib.Command exposing (Command, apply, batch, create, destroy, idle, send, store, transition)

import Grid.Direction exposing (Direction)


type SingleCommand a c
    = Store
    | Send Direction
    | Create c
    | Transition a
    | Destroy


type Command a c
    = Command (List (SingleCommand a c))


batch : List (Command a c) -> Command a c
batch =
    List.foldr
        (\(Command l) ->
            List.append l
        )
        []
        >> Command


idle : Command a c
idle =
    Command []


create : c -> Command a c
create =
    Create
        >> List.singleton
        >> Command


store : Command a c
store =
    Store
        |> List.singleton
        |> Command


send : Direction -> Command a c
send =
    Send
        >> List.singleton
        >> Command


transition : a -> Command a c
transition =
    Transition
        >> List.singleton
        >> Command


destroy : Command a c
destroy =
    Destroy
        |> List.singleton
        |> Command


apply :
    { store : a -> Result x a
    , send : Direction -> a -> Result x a
    , transition : b -> a -> Result x a
    , create : c -> a -> Result x a
    , destroy : a -> Result x a
    }
    -> Command b c
    -> a
    -> a
apply fun (Command command) b =
    command
        |> List.filterMap
            (\c ->
                b
                    |> (case c of
                            Store ->
                                fun.store

                            Send direction ->
                                fun.send direction

                            Transition sort ->
                                fun.transition sort

                            Create item ->
                                fun.create item

                            Destroy ->
                                fun.destroy
                       )
                    |> Result.toMaybe
            )
        |> List.head
        |> Maybe.withDefault b
