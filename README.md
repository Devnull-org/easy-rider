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

This project aims to ease the pain of running the cardano-node. It is advised
to user mithril-client to download the snapshot from which it is a lot faster
to bootstrap cardano-node. Easy Rider merges these two steps into one and
provides options to run cardano-node on all three networks: mainnet, preprod or
preview.

### Usage 

You can download Easy Rider binary from the release
(page)[https://github.com/Devnull-org/easy-rider/releases].

Run the `--help` to view available network options.

```

./easy-rider run-node --help

```

Once started `easy-rider` will use `mithril-client` under the hood to download
and verify cardano snapshot and then start the cardano-node. The easiest is to
start the binary from the location of the cloned repo since needed
configuration files for mithril and cardano-node are already there.

When you start `easy-rider` it will use `mithril` to download, verify and
unpack the snapshot into a `db` folder. If the `db` folder is present at the
root of the project `easy-rider` will **NOT** run `mithril`. It will assume
your db is already in sync so you need to make sure to use the same network
again or delete the `db` folder.

### Goals

- [x] Run cardano-node and sync it as fast as possible
- [ ] Provide option to submit a transaction to local running cardano-node 
- [ ] Run Hydra 

### Tasks

- [x] Use mithril to speed up the node sync time
- [x] Run local cardano-node on specified network 

### For dApp builders 

If you use nix (hopefully) then using this app is a breeze since you have all
of the dependencies in scope in a nix shell. 

```bash
nix develop

cabal run easy-rider
```

If you don't use nix then you need to provide the dependencies yourself. The
app depends on `mithril-client 2337.0`, `cardano-node 8.1.2`,  `ghc 9.2.8` and
`cabal 3.0`.
