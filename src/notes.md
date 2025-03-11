




handles for referencing background tasks

which include

repeat streams
one time fires
and timer based callbacks


handles are needed to allow consumers to de-register registered background tasks

handles should be distinct strings that a client creates

in the event that a background task with a duplicate handle name is created, hard crash immediately


