# Neuro-Evolutional "Don't Touch the Spikes" with Erlang
# by Tamir Cohen & Nadav Hadad


## Installation
we are using erlang version OTP 25 on ubuntu 20.04.
Use the package manager to install erlang:
```bash
sodu apt-get install erlang
```

## Usage
you should open a console inside the src folder.
there are multiple ways you can run the program:

### single console
to run the code on a single console change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE,  'pc0@<enter pc0 hostname>').
-define(PC1_NODE, 		'pc0@<enter pc0 hostname>').
-define(PC2_NODE, 		'pc0@<enter pc0 hostname>').
-define(PC3_NODE, 		'pc0@<enter pc0 hostname>').
-define(PC4_NODE, 		'pc0@<enter pc0 hostname>').
```
then from a bash terminal run the following command:
```bash
erl -sname 'pc0' -setcookie yummy
```
then move to the Run instructions.

### five consoles on one computer
to run the code on five consoles change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE,  'pc0@<enter pc0 hostname>').
-define(PC1_NODE, 		'pc1@<enter pc0 hostname>').
-define(PC2_NODE, 		'pc2@<enter pc0 hostname>').
-define(PC3_NODE, 		'pc3@<enter pc0 hostname>').
-define(PC4_NODE, 		'pc4@<enter pc0 hostname>').
```
then in each bash terminal go to the src directory and run the following command:
```bash
erl -sname 'pc<x>' -setcookie yummy
```
where <x> is indexed between 0-4.

then move to the Run instructions.

### five consoles on five different computers
to run the code on a five computers change the configuration in src/Constants.hrl to:

```erlang
-define(GRAPHICS_NODE,  'pc1@<enter pc0 ip>').
-define(PC1_NODE, 		'pc1@<enter pc1 ip>').
-define(PC2_NODE, 		'pc2@<enter pc2 ip>').
-define(PC3_NODE, 		'pc3@<enter pc3 ip>').
-define(PC4_NODE, 		'pc4@<enter pc4 ip>').
```

then in each bash terminal run the following command:
```bash
erl -name 'pc<x>@<pc ip address>' -setcookie yummy
```
where <x> is indexed between 0-4 and <pc ip address> is the ip of the computer which runs the code.

then move to the Run instructions.

## Run
to run the code in each of your open consoles run the following command:
```erlang
cover:compile_directory().
```
to compile the code.

then only in the console of pc0, run the following command:
```erlang
graphics:start().
```

### all together for convenient
erl -sname 'pc0' -setcookie yummy
cover:compile_directory().
graphics:start().


