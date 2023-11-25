```
 ______   ________   ______   __  __     ______     ________  ______   ______   ______       
/_____/\ /_______/\ /_____/\ /_/\/_/\   /_____/\   /_______/\/_____/\ /_____/\ /_____/\      
\::::_\/_\::: _  \ \\::::_\/_\ \ \ \ \  \:::_ \ \  \__.::._\/\:::_ \ \\::::_\/_\:::_ \ \     
 \:\/___/\\::(_)  \ \\:\/___/\\:\_\ \ \  \:(_) ) )_   \::\ \  \:\ \ \ \\:\/___/\\:(_) ) )_   
  \::___\/_\:: __  \ \\_::._\:\\::::_\/   \: __ `\ \  _\::\ \__\:\ \ \ \\::___\/_\: __ `\ \  
   \:\____/\\:.\ \  \ \ /____\:\ \::\ \    \ \ `\ \ \/__\::\__/\\:\/.:| |\:\____/\\ \ `\ \ \ 
    \_____\/ \__\/\__\/ \_____\/  \__\/     \_\/ \_\/\________\/ \____/_/ \_____\/ \_\/ \_\/ 
                                                                                             
```

# Easy Rider 

This project aims to ease the life of cardano developers in the wild. Cardano
is hard to work with and the goal is to make it just a little bit approachable,
and do that step by step.


### Goals

- [x] Run cardano-node and sync it as fast as possible
- [ ] Provide option to submit a transaction to local running cardano-node 
- [ ] Run cardano-node and sync it as fast as possible

### Tasks

- [x] Use mithril to speed up the node sync time
- [x] Run local cardano-node on specified network 
- [ ] Use proper logging library 
- [ ] Set up CI server

### Developing 

If you use nix (hopefully) then using this app is a breeze since you have all
of the dependencies in scope in a nix shell. 

```bash
nix develop

cabal run cardano-lab
```

If you don't use nix then you need to provide the dependencies yourself. The
app depends on `mithril-client 0.3.32`, `cardano-node 8.1.2`,  `ghc 9.2.8` and
`cabal 3.0`.
