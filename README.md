# Epistemic Group Gossip


## Running the app

### Precompiled executable

An executable is available for Linux and macOS. These are available under [Releases](https://github.com/RamonMeffert/epistemic-group-gossip/releases). Before running, you have to allow executing the file using `chmod +x egg-{os}` where `{os}` is `macOS` or `Linux`. When running on macOS, you might get a warning that the file is not signed (’“egg-macOS” cannot be executed because the developer cannot be verified’). To get around this, open System Preferences » Security & Privacy » General and click “Allow anyway”. Then, you can run the app using `./egg-{os}` (where `{os}` is `macOS` or `Linux` again) from the directory it is in.

### Building and running manually

#### Building

If you want to try the tool, make sure you have the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) installed first. After that, you can run `stack build` to build the project (might take a couple of minutes the first time) and `stack run` to run the project.

#### Running

Currently when you run `stack run` you are presented with a command-line interface (CLI) in which you can work on two predefined gossip graphs. You can

1. Execute calls
2. View possible calls
3. View the current state

The current state will show you the current gossip graph, the current knowledge structure (WIP) and the call sequence so far.

---

<sub>
Favicon for the Github Pages site by <a href="https://www.flaticon.com/authors/eucalyp" title="Eucalyp">Eucalyp</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a>
</sub>
