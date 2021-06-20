# SingOut

A wrapper for the IO monad that instead of writing 
sings its output in Solresol, using the Euterpea
library for MIDI playback.

Supported singables include:
* integers (Haskell's Int type)
* strings (in case-insensitive Solresol latin notation, separated with spaces)
* Euterpea's Music structures 

## Examples

Each example, if run with no arguments, will display 
the list of devices available for MIDI input/output.

```
>> ./MariaAndDraco

Input devices: 
  InputDeviceID 1	Midi Through Port-0

Output devices: 
  OutputDeviceID 0	Midi Through Port-0
  OutputDeviceID 2	TiMidity port 0
  OutputDeviceID 3	TiMidity port 1
  OutputDeviceID 4	TiMidity port 2
  OutputDeviceID 5	TiMidity port 3
  OutputDeviceID 6	TiMidity port 0
  OutputDeviceID 7	TiMidity port 1
  OutputDeviceID 8	TiMidity port 2
  OutputDeviceID 9	TiMidity port 3
```

When a valid `OutputDeviceID` is provided as the first (and only)
argument, the program will run normally, executing its Singer.

### Fib (executable: Fibonacci)

A simple program naively computing the first 30 Fibonacci numbers
(considering 0 as the 0-th Fibonacci number) and singing them out.

This presents how musical output is independent from the computations.

Known bug: when the number of computed numbers is set to 
a large value, there are issues with playback (signals to the driver
are not sent at short enough intervals, resulting in a distorted,
very slow output).
 
### Fork

An example program showing how the Singer can be used
in concurrent programming.

The program spawns two threads:
* one thread sings "do" in an infinite loop
* one thread sings "si" in an infinite loop

The result is an undeterministic sequence of C and B notes.

### Poem (executable: LaRedomifa)

The program reads a short love poem called 'La Redomifa'
written in the Latin notation of Solresol.

The poem can be found [here](http://love.poem.free.fr/constructed-poems/solresol-poem.html).

### Opera (executable: MariaAnDraco)

A custom rendition of 'Aria di Mezzo Carattere', 
a part of the opera sequence from Final Fantasy VI 
(a.k.a. Final Fantasy III).

It combines the multi-threading presented in **Fork** 
with the solresol reading capabilities presented in **Poem**.

The MIDI file is taken from [fflyrics.com](http://www.fflyrics.com/ff6.html).
