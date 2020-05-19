# Programming challenge: indexing a file

### Indexing a file

The aim of this exercise is to index a text file, by line number. We can think of the input being a list of text strings, and below we’ve provided an outline Erlang module that reads text files into this format, as well as a couple of example files to process.

The output of the main function should be a list of entries consisting of a word and a list of the ranges of lines on which it occurs.

For example, the entry

```
{ "foo" , [{3,5},{7,7},{11,13}] }
```

means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.

To take the problem further, you might like to think about these ways of refining the solution.

* Removing all short words (e.g. words of length less than 3) or all common words (you‘ll have to think about how to define these).

* Sorting the output so that the words occur in lexicographic order.

* Normalising the words so that capitalised ("Foo") and non capitalised versions ("foo") of a word are identified.

* Normalising so that common endings, plurals etc. identified.

* (Harder) Thinking how you could make the data representation more efficient than the one you first chose. This might be efficient for lookup only, or for both creation and lookup.

* Can you think of other ways that you might extend your solution?

### Example

* Short 

```
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> c(index).
{ok,index}
2> index:file("gettysburg-address.txt").
[{"above",[{16,16}]},
 {"advanced",[{20,20}]},
 {"altogether",[{10,10}]},
 {"battlefield",[{7,7}]},
 {"before",[{22,22}]},
 {"birth",[{26,26}]},
 {"brave",[{15,15}]},
 {"brought",[{1,1}]},
 {"cause",[{23,23}]},
 {"civil",[{5,5}]},
 {"come",[{8,8}]},
 {"conceived",[{2,6}]},
 {"consecrate",[{14,14}]},
 {"consecrated",[{16,16}]},
 {"continent",[{2,2}]},
 {"created",[{3,3}]},
 {"dead",[{15,22}]},
 {"dedicate",[{8,13}]},
 {"dedicated",[{19,21},{3,6}]},
 {"detract",[{16,16}]},
 {"devotion",[{23,24}]},
 {"died",[{25,25}]},
 {"earth",[{28,28}]},
 {"endure",[{7,7}]},
 {"engaged",[{5,...}]},
 {"equal",[{...}]},
 {"fathers",[...]},
 {[...],...},
 {...}|...]
```

* Long

```
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> c(index).
{ok,index}
2> index:file("dickens-christmas.txt"). 
[{"abed",[{2270,2270}]},
 {"abels",[{570,570}]},
 {"abide",[{2703,2703}]},
 {"abject",[{2671,2671}]},
 {"able",[{1491,3317}]},
 {"abode",[{849,849}]},
 {"about",
  [{3693,3740},
   {3527,3659},
   {3068,3295},
   {2882,3052},
   {2782,2795},
   {2711,2742},
   {2549,2586},
   {2430,2505},
   {2323,2364},
   {2273,2277},
   {2166,2167},
   {1967,2031},
   {1584,1773},
   {1254,1403},
   {1227,1243},
   {1017,1104},
   {909,1007},
   {770,795},
   {644,...},
   {...}|...]},
 {"above",[{2352,2376},{1949,2343},{61,518}]},
 {"abrahams",[{572,572}]},
 {"abroad",[{743,1871}]},
 {"abstinence",[{3773,3773}]},
 {"abundance",[{331,331}]},
 {"abyss",[{2394,2394}]},
 {"accommodate",[{1999,1999}]},
 {"accompanied",[{1281,1351}]},
 {"account",[{3015,3695},{3004,3010}]},
 {"accuracy",[{3091,3091}]},
 {"accustomed",[{2883,3723}]},
 {"ache",[{3054,3054}]},
 {"achieved",[{2163,2163}]},
 {"acquaintance",[{2407,2407}]},
 {"acquainted",[{1736,1736}]},
 {"across",[{1173,3624},{522,582}]},
 {"acted",[{1450,1450}]},
 {"action",[{409,...}]},
 {"active",[{...}]},
 {"adamant",[...]},
 {[...],...},
 {...}|...]
```