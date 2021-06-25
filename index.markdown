---
layout: home
title: Epistemic Gossip with Group Calls
menu: Home
---

## Quick links

<div style="columns: 3 225px;">
<div markdown="block" style="background-color: #def; padding: 1rem 1rem 0.1rem 1rem;margin-bottom: 1rem;break-inside: avoid">
### [Theoretical Background]({{ site.baseurl }}{% link theory/background.markdown %})

A summary of the theoretical background necessary for understanding this project.

<a href="{{ site.baseurl }}{% link theory/background.markdown %}" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;">→ Background</a>
</div>
<div markdown="block" style="background-color: #def; padding: 1rem 1rem .1rem 1rem;margin-bottom: 1rem;break-inside: avoid;">
### [This project]({{ site.baseurl }}{% link theory/this-project.markdown %})

A theoretical description of our extensions to gossip, knowledge structures and action models.

<a href="{{ site.baseurl }}{% link theory/this-project.markdown %}" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;">→ This project</a>
</div>
<div markdown="block" style="background-color: #def; padding: 1rem 1rem .1rem 1rem;margin-bottom: 1rem; break-inside: avoid">
### [Documentation]({{ site.baseurl }}{% link docs/index.html %})

Automatically generated Haddock documentation based on comments in the Haskell source code.

<a href="{{ site.baseurl }}{% link docs/index.html %}" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;">→ Documentation</a>
</div>
</div>
<div markdown="block" style="background-color: #def; padding: 1rem 1rem .1rem 1rem; margin-bottom: 1rem;">
### Source code and executable

A pre-compiled executable is available for macOS and Linux. The source code is available on GitHub.

<details markdown="block" style="margin-bottom:1rem;">
<summary>Running</summary>

You can use the program by running `./egg-{version}` in a terminal window. On macOS, you might have to [remove the quarantine flag](https://proinsias.github.io/til/Mac-Remove-quarantine-flag-from-app/) first.
</details>

<a href="https://github.com/RamonMeffert/epistemic-group-gossip/releases/download/latest/Egg-Linux" target="_blank" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;">
⤓ Download for Linux
</a>
<a href="https://github.com/RamonMeffert/epistemic-group-gossip/releases/download/latest/Egg-macOS" target="_blank" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;">
⤓ Download for macOS
</a>
<a href="https://github.com/RamonMeffert/epistemic-group-gossip" target="_blank" style="padding: .4rem 1rem; background-color: #7da0c445; border-radius: 3px; display: inline-block; color: black;"><svg style="width:16px; height: 16px; display: inline-block; vertical-align:-2px;"><use xlink:href="/epistemic-group-gossip/assets/minima-social-icons.svg#github"></use></svg> Check out the source code
</a>
</div>

## Introduction

Often, in a social network, agents have certain knowledge that others do not. In gossip these pieces of knowledge are so-called secrets. There are multiple ways of spreading these secrets through the network so that each agent eventually knows every secret. This is where gossip protocols come in.

Multiple gossip protocols already exist that try to find the most efficient way of spreading the secrets through the network. The "basic" versions of these protocols do not model knowledge, although several extensions of existing protocols as well as completely new protocols exist that *do* use epistemic modelling.

## The project

This project aims to involve higher-order epistemic knowledge by adding _group calls_ to Gossip graphs. Group calls allow multiple agents share their secrets in one call, therefore possibly speeding up knowledge spreading through the network. Group calls could therefore be interpreted as so-called _public announcements_ from epistemic logic. Hence, this model might be used as a tool to study the interactions of users on multiple types of social media, where these interactions might be characterized as group calls or public announcements (e.g. Facebook posts, Zoom calls, etc.)

However, as group-calls might involve extra efforts and/or resources it is not always the 'best' action to perform if, for example, most of the agents involved in the group call already know the secrets about to be shared. To this end, the agents may be more able to judge the appropriate action if they keep track of the knowledge of the other agents.

This model monitors the knowledge of the agents in the network and allows the user to have the agents perform certain actions that might change the knowledge distribution of the network and observe these changes. Optionally, some gossip protocols will be modified, allowing them to execute group calls, and implemented so that their effect on the distributed knowledge might be studied.

### Model layout

The model is programmed in Haskell as it is a functional programming language and therefore allows us to define the gossip graph structure and its operators through functions, who will be called appropriately. As these functions do not differ much from the formal notation will this not only speed up the conversion from this formal notation into 'functional' code but also allow easier interpretation and explanation of the implementation. Furthermore, the Action Models from SMCDEL (written in Haskell) can be imported and modified, so that less time is spend on implementing the gossip graphs and more time can be spend implementing the group calls, perchance increasing model performance.

## Model interaction

> The notation used for denoting agents and their knowledge of the numbers and secrets of the other agents follows the notation used by [Van Ditmarsch (2017)](https://doi.org/10/f9p6c3)

Interacting with the model is quite intuitive. All possible inputs each time are either highlighted in the console, or explained. Invalid input is detected and caught to prevent undesired behaviour. We identify the following control flow / interaction parts:

### Model layout

The user will first have to provide the layout of the gossip graph. This layout will determine which agents will have the numbers of which other agents initially. The user can either choose to use a predefined layout (defined by us), choose to load a layout from a text file or to type it out in the console input directly.

### Model operation mode

Once the graph is initialized, the user can choose the model operation mode; *user actions*, *protocol* or *hybrid* mode. *User actions* mode allows the user to interact each tick. *Protocol* mode will run the model using the gossip protocol specified by the user. Finally, the *hybrid* mode is a mixture between the *user actions* and *protocol* mode, where each tick, the user may choose whether to execute a *user action*, *protocol action* or perform both.

### Actions
As for the *user actions*, the user can choose between multiple actions. First action is to perform a *call*. Secondly, the user can view the possible calls. Finally there is the possibility of viewing the current state of the model.

During a *protocol action*, the model will first determine all the calls that are allowed. This includes all the single calls (i.e. all pairs of $$xy$$ such that $$Nxy$$), and group calls (i.e. all $$xY$$ such that $$x\in N$$ and $$Y\subseteq (N\setminus x)$$ and $$\forall y\in Y\ :\ Nxy$$). Then, the protocol is used to filter those calls that can be made using the rule defined in the protocol (e.g. for the learn-new-secrets (LNS) protocol this are all the calls $$(x,y)$$ such that $$Nxy\wedge\neg Sxy$$). Once there are both direct- and group calls left, the user may choose which type of call is actually executed, otherwise the one type is chosen. Then, the first call of this type is performed. 

### Making a call

Making a call is very simple. Once the user select they want to make a call, the user first has to enter the name of the agent who is calling (i.e. *a* for agent 0). Then the user can enter the names of the agents who are being called. If only one agent is entered, a direct call will be made. If multiple agents are entered (e.g. *bcd*) a group call will be made.

> Note that for both types of calls, the calls have to be valid (i.e. the agent who is calling needs to have the number of all the agents that are being called). However, the program will check for that and will show which call is invalid and ask for a resubmission.

### Viewing the model state

Either by user request, or after each tick, the state of the model is shown. This will include 3 main properties of the model state: the gossip graph, the knowledge structure and the call sequence. The gossip graph will show all the different connections between the agents. The knowledge structure includes the display of the vocabulary, event law and observables per agent. Finally the call sequence will show all the calls that have been made (note that as group calls are converted to multiple direct calls, the call sequence will only contain direct calls).

### Complete state / Model termination

After each update the of the model state, the model is checked on completion. A model is complete whenever every agent is an expert: every agent knows the secret of every other agent. If this is the case the model will terminate ending the program. However, the user may choose to keep on interacting with the model if they want to.
