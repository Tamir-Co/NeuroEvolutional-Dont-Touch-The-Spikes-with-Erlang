# NeuroEvolutional "Don't Touch The Spikes" with Erlang
# by Tamir Cohen & Nadav Hadad


## Installation
we are using erlang version 25 on ubuntu. this will probably also work on windows but it was not tested.
Use the package manager to install erlang:
```bash
sodu apt-get install erlang
```

and your ready to go!
## Usage
there are multiple ways you can run the program:

### single console
to run the code on a single console change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc0@<enter pc0 hostname>').
-define(PC1, 'pc0@<enter pc0 hostname>').
-define(PC2, 'pc0@<enter pc0 hostname>').
-define(PC3, 'pc0@<enter pc0 hostname>').
-define(PC4, 'pc0@<enter pc0 hostname>').
```
then from a bash terminal run the following command:
```bash
erl -sname 'pc0' -setcookie yummy
```
next move to the Run instructions.

### four consoles on one computer
to run the code on four consoles change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc1 hostname>').
-define(PC1, 'pc1@<enter pc1 hostname>').
-define(PC2, 'pc2@<enter pc1 hostname>').
-define(PC3, 'pc3@<enter pc1 hostname>').
-define(PC4, 'pc4@<enter pc1 hostname>').
```
then in each bash terminal go to the src directory and run the following command:
```bash
erl -sname 'pc<x>' -setcookie yummy
```
where <x> is indexed between 1-4.

next move to the Run instructions.

### four consoles on four different computers
to run the code on a four computers change the configuration in src/Constants.hrl to:

```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc1 ip>').
-define(PC1, 'pc1@<enter pc1 ip>').
-define(PC2, 'pc2@<enter pc2 ip>').
-define(PC3, 'pc3@<enter pc3 ip>').
-define(PC4, 'pc4@<enter pc4 ip>').
```

then in each bash terminal run the following command:
```bash
erl -name 'pc<x>@<pc ip address>' -setcookie yummy
```
where <x> is indexed between 1-4 and <pc ip address> is the ip of the computer which runs the code.

next move to the Run instructions.

## Run
to run the code in each of your open consoles run the following command:
```erlang
cover:compile_directory().
```
to compile the code.
then only on the console which is pc1 run the following command:
```erlang
graphics:start().
```

### all together
erl -sname 'pc0' -setcookie yummy
cover:compile_directory().
graphics:start().


