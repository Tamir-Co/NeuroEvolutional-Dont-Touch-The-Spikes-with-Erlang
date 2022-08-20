# Neuro-Evolutional "Don't Touch the Spikes" with Erlang
# By Tamir Cohen & Nadav Hadad


## Installation
We are using erlang version OTP 25.03 on ubuntu 20.04.
Use the package manager to install erlang:
```bash
sodu apt-get install erlang
```

## Usage
You should open consoles inside the src folder.
There are multiple ways you can run the program:

### single console in one computer
Change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc0@<enter pc0 hostname>').
-define(PC1_NODE,      'pc0@<enter pc0 hostname>').
-define(PC2_NODE,      'pc0@<enter pc0 hostname>').
-define(PC3_NODE,      'pc0@<enter pc0 hostname>').
-define(PC4_NODE,      'pc0@<enter pc0 hostname>').
```
Then from a bash terminal run the following command:
```bash
erl -sname 'pc0' -setcookie yummy
```
Then move to the Run instructions.

### five consoles in one computer
Change the configuration in src/Constants.hrl to:
```erlang
-define(GRAPHICS_NODE, 'pc0@[enter pc0 hostname]').
-define(PC1_NODE,      'pc1@<enter pc0 hostname>').
-define(PC2_NODE,      'pc2@<enter pc0 hostname>').
-define(PC3_NODE,      'pc3@<enter pc0 hostname>').
-define(PC4_NODE,      'pc4@<enter pc0 hostname>').
```
Then in each bash terminal go to the src directory and run the following command:
```bash
erl -sname 'pc[x]' -setcookie yummy
```
Where [x] is indexed between 0-4.

Then move to the Run instructions.

### five consoles in five different computers
Change the configuration in src/Constants.hrl to:

```erlang
-define(GRAPHICS_NODE, 'pc1@<enter pc0 ip>').
-define(PC1_NODE,      'pc1@<enter pc1 ip>').
-define(PC2_NODE,      'pc2@<enter pc2 ip>').
-define(PC3_NODE,      'pc3@<enter pc3 ip>').
-define(PC4_NODE,      'pc4@<enter pc4 ip>').
```

Then in each bash terminal run the following command:
```bash
erl -name 'pc(x)@<pc ip address>' -setcookie yummy
```
Where (x) is indexed between 0-4 and <pc ip address> is the ip of the computer which runs the code.

Then move to the Run instructions.

## Run
To run the code in each of your open consoles run the following command:
```erlang
cover:compile_directory().
```
to compile the code.

Then only in the console of pc0, run the following command:
```erlang
graphics:start().
```

### All together for convenient
```erlang
erl -sname 'pc0' -setcookie yummy
cover:compile_directory().
graphics:start().
```