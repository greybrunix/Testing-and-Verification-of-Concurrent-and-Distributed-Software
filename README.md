# LCC Project 11

## API

 - construct(dup,drop,hist)
    $$construct: \mathbb{B}^3 \rightarrow S()$$
    a function that receives a triple and alters the state of the program,
    allowing duplications, drops or keeping history (not deleting messages on receives)
    - Examples:
        `construct(1,0,1)` will activate duplications and preserve history</br>
        `construct(0,0,0)` will run on default settings with no dups, drops or history</br>
        `construct(0,1,0)` will activate drops</br>
        `construct(0,1,1)` will activate drops and history resulting in drops also removing
        from the history
 - get\_net() -> net
    $$get\_{net}: () \rightarrow S(MSet + List)$$
    Function that returns the current state of the network
 - send(src, dst, payload)
    $$send: Src \times Dst \times Payload \rightarrow S()$$
    a function that receives three arguments of arbitrary types that correspond to the
    node the sent the message (src), who is intended to receive the message (dst)
    and the contents of the message itself (payload)
 - receive(dst) -> msg
    $$receive: Dst \rightarrow S(Payload)$$
    Function that receives who is intended to receive a message and returns a message
    that was sent to that client; Note, if hist is set to False (default), receiving a message
    removes it from the Mailbox, drop may result in an empty message and dup will add another
    instance of the message to the Mailbox;
    If there are no messages addressed to dst in the network, then the function returns an empty
    dictionary.

## Visualization

 - python graph.py filename.(hny|hco) (--noweb)?  
   Runs either the .hny or the .hco file, it can also have the --noweb flag just as in harmony.
