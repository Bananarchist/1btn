module Queue exposing (Queue, clear, dequeue, dequeued, enqueue, enqueued, map)


type alias Queue a =
    List a


{-| Add item to end of queue
-}
enqueue : Queue a -> a -> Queue a
enqueue q element =
    List.append q [ element ]


{-| First queued item
-}
dequeue : Queue a -> Maybe a
dequeue =
    List.head


{-| Queue after dequeuing
-}
dequeued : Queue a -> Queue a
dequeued =
    List.drop 1


{-| Any elements in queue?
-}
enqueued : Queue a -> Bool
enqueued =
    List.length >> (==) 0 >> not


{-| Apply function to first item in queue
-}
map : (a -> a) -> Queue a -> Queue a
map fn q =
    case q of
        x :: xs ->
            fn x :: xs

        _ ->
            q


{-| Empty out queue
-}
clear : Queue a -> Queue a
clear =
    always []
