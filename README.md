```
   ______               __                     __          __  
  / ____/___ __________/ /___ _____  ____     / /   ____ _/ /_ 
 / /   / __ `/ ___/ __  / __ `/ __ \/ __ \   / /   / __ `/ __ \
/ /___/ /_/ / /  / /_/ / /_/ / / / / /_/ /  / /___/ /_/ / /_/ /
\____/\__,_/_/   \__,_/\__,_/_/ /_/\____/  /_____/\__,_/_.___/ 
                                       
```

# Cardano Lab

This project aims to ease the life of cardano developers in the wild. Cardano
is hard to work with and the goal is to make it just a little bit approachable,
and do that step by step.


### Goals

- [x] Run cardano-node and sync it as fast as possible
- [ ] Provide option to submit a transaction to local running cardano-node 

### Tasks

- [ ] Set up CI server
- [ ] Write test that cover all use cases
- [x] Run local cardano-node on specified network 
- [x] Use mithril to speed up the node sync time
- [ ] Use proper logging library 

### Usage

If you use nix (hopefully) then using this app is a breeze since you have all
of the dependencies in scope in a nix shell. 

```bash
nix develop

cabal run cardano-lab
```

If you don't use nix then you need to provide the dependencies yourself. The
app depends on `mithril-client 2329.0`, `cardano-node 8.1.2`,  `ghc 9.2.8` and
`cabal 3.0`.
