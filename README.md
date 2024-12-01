# Said's NFA-Algorithm
## Description
This Algorithm solves the question, if a word is accpted or rejected by an Non-deterministic Finite Automaton (NFA).

## Procedure
Let M = (Q, Σ, Ɛ, q, δ, F) be an NFA, where Ɛ is a placeholder for the empty word.
To calculate if any word is accepted by the NFA, we run the algorithm with the word
as first input and the final states as declared as second input.

The Algorithm checks works in following steps:

1. Initialize a 'generation' G with the initial state as only member.
   It can be represented as set or boolean Array.
2. for each letter i in the input word
    1. Update G to be only the states that are reachable with the input 'i',
        by checking every outgoing connection of G that have 'i' as input and following states that have 'epsilon' as input.
        Do not consider following epsilon connection that lead to already checked states, so called 'forbidden states'.
3. Finally update G, so every state that is reachable with 'epsilon' inputs is contained in the list.
   Do not consider following epsilon connection that lead to already checked states, so called 'forbidden states'.
4. Check if G contains one of the final states listed in F. If yes the word has been accepted, else it's been rejected.

## Runtime
We can estimate the runtime to be linear in the word input, due to it only influencing a for-loop.
The runtime regarding the amount of states would be linear as well, due to us being able
to inspect states in O(1) of time to verify already visited states and only checking outgoing connection of each state maximally once
in each for-loop step (and in the 3rd step). The maximal amount of connections we need to check for each state is the amount of states
itself multiplied by two, because every state can be connected by either a connection with the letter as input or epsilon.
In the very last step we additonally check every final state, if it's contained in the generation. This can be realised by m Array-lookups,
which is linear to m.

To summarize we have a runtime of O(nm^2), where n is the length of the word and m is the amount of states we have.
Sometimes it makes sense to realise the same Algorithm with a set, just like in the provided implementation,
due to a software paradigm, space efficiency or being simpler. The runtime would then be O(nlog(m)m^2) which is element of O(nm^3).

So we have a guaranteed, this Algorithm's runtime is linear in regards to the input word and polynomial in regards to the amount of states.

## Conclusion
The naive approach to solve whether a word is being accepted or rejected by an NFA is translating the NFA
to a Deterministic Finite Automaton (DFA) or to traverse the transitions of specified by the input with
guess-work and backtracking. The first approach exponential amount of storage and time when building the DFA and
the second approach requires exponential amount of time for every word that is being checked.
By introducing the idea of 'generations' and limitating the search to these, we can improve the runtime dramastically,
while still keeping a low memory footprint.
