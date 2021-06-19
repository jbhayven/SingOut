# SingOut

An wrapper for IO monad that instead of writing 
sings its output in Solresol, using the Euterpea
library for MIDI playback.

Supported singables include:
* integers (Haskell's Int type)
* strings (in case-insensitive Solresol latin notation, separated with spaces)
* Euterpea's Music structures 

## Examples

### Fork

An example program showing how the Singer can be used
in concurrent programming.

The program spawns two threads:
* one thread sings "do" in an infinite loop
* one thread sings "si" in an infinite loop

The result is an undeterministic sequence of C and B notes.

### Opera

A custom rendition of 'Aria di Mezzo Carattere', 
a part of the opera sequence from Final Fantasy VI 
(a.k.a. Final Fantasy III).

The MIDI file was taken from [fflyrics.com](http://www.fflyrics.com/ff6.html).

### Poem

The program reads a short love poem called 'La Redomifa'
written in the Latin notation of Solresol.

The poem can be found [here](http://love.poem.free.fr/constructed-poems/solresol-poem.html).
