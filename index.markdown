---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
title: Epistemic Gossip with Group Calls
menu: Home
---

## Quick links

- For our planning and to-do items, please refer to the [About]({{ site.baseurl }}{% link about.markdown %}) page.
- For the theoretical background for this project, please refer to the [Theoretical Background]({{ site.baseurl }}{% link theory/background.markdown %}) page.
- For a theoretical description of our extensions to gossip, knowledge structures and action models, please refer to the [This project]({{ site.baseurl }}{% link theory/this-project.markdown %}) page.
{: style="background-color: #def; padding: 1rem 1.4rem 1rem 2rem; margin:0 0 1rem 0"}

## Introduction

Often, in a social network, agents have certain knowledge that others do not. In gossip these pieces of knowledge are so-called secrets. There are multiple ways of spreading these secrets through the network so that each agent eventually knows every secret. This is where gossip protocols come in.

Multiple Gossip protocols already exist that try to find the most efficient way of spreading the secrets through the network. However, none are modelled to incorporate (higher-order) epistemic knowledge.

## The project

This project aims to involve higher-order epistemic knowledge by adding _group calls_ to Gossip graphs. Group calls allow multiple agents share their secrets in one call, therefore possibly speeding up knowledge spreading through the network. Group calls could therefore be interpreted as so-called _public announcements_ from epistemic logic. Hence, this model might be used as a tool to study the interactions of users on multiple types of social media, where these interactions might be charactarized as group calls or public announcements (e.g. Facebook posts, Zoom calls, etc.)

However, as group-calls might involve extra efforts and/or resources it is not always the 'best' action to perform if, for example, most of the agents involved in the group call already know the secrets about to be shared. To this end, the agents may be more able to judge the appropriate action if they keep track of the knowledge of the other agents.

This model monitors the knowledge of the agents in the network and allows the user to have the agents perform certain actions that might change the knowledge distribution of the network and observe these changes. Optionally, some gossip protocols will be modified, allowing them to execute group calls, and implemented so that their effect on the distributed knowledge might be studied.

### Model layout

The model is programmed in Haskell as it is a functional programming language and therefore allows us to define the gossip graph structure and its operators through functions, who will be called appropriately. As these functions do not differ much from the formal notation will this not only speed up the conversion from this formal notation into 'functional' code but also allow easier interpretation and explanation of the implementation. Furthermore,  the Action Models from SMCDEL (written in Haskell) can be imported and modified, so that less time is spend on implementing the gossip graphs and more time can be spend implementing the group calls, perchance increasing model performance.

## Model interaction
> The notation used for denoting agents and their knowledge of the numbers and secrets of the other agents follows the notation used by van Dietmarsch [[1]](https://doi.org/10/f9p6c3)

Interacting with the model is quite inuitive. All possible inputs each time are either highlighted in the console, or explained. Invalid input is detected and catched to prevent undesired behaviour. We indentify the following control flow / interaction parts:

### Model layout
The user will first have to provide the layput of the gossip graph. This layout will determine which agents will have the numbers of which other agents intially. The user can either choose to use a predefined layout (defined by us), choose to load a layout from a text file or to type it out in the console input directly.

### Model operation mode
Once the graph is initialized, the user can choose the model operation mode; *user actions*, *protocol* or *hybrid* mode. *User actions* mode allows the user to interact each tick. *Protocol* mode will run the model using the gossipprotocol specified by the user. Finally, the *hybrid* mode is a mixture between the *usera ctions* and *protocol* mode, where each tick, the user may choose whether to execute a *user action*, *protocol action* or perform both.

### Actions
As for the *useractions*, the user can choose between multiple actions. First action is to perform a *call*. Secondly, the user can view the possible calls. Finally there is the possibility of viewing the current state of the model.

During a *protocol action*, the model will first determine all the calls that are allowed. This includes all the single calls (i.e. all pairs of $xy$ such that $Nxy$), and groupcalls (i.e. all $xY$ such that $x\in N$ and $Y\subseteq (N\setminus x)$ such that $\forall y\in Y\ :\ Nxy$). Then, the protocol is used to filter those calls that can be made using the rule defined in the protocol (e.g. for the learn-new-secrets (LNS) protocol this are all the calls $(x,y)$ such that $Nxy\wedge\neg Sxy$). Once there are both direct- and groupcalls left, the user may choose which type of call is actually executed, otherwise the one type is chosen. Then, the first call of this type is performed. 

### Making a call
Making a call is very simple. Once the user select they want to make a call, the user first has to enter the name of the agent who is calling (i.e. *a* for agent 0). Then the user can enter the names of the agents who are being called. If only one agent is entered, a direct call will be made. If multiple agents are entered (e.g. *bcd*) a groupcall will be made.

> Note that for both types of calls, the calls have to be valid (i.e. the agent who is calling needs to have the number of all the agents that are being called). However, the program will check for that and will show which call is invalid and ask for a resubmission.

### Viewing the modelstate
Either by user request, or after each tick, the state of the model is shown. This will include 3 main properties of the modelstate: the gossipgraph, the knowledgestructure and the callsequence. The gossipgraph will show all the different connections between the agents. The knowledgestructure includes the display of the vocabulary, eventlaw and observables per agent. Finally the callsequence will show all the calls that have been made (note that as groupcalls are converted to multiple direct calls, the callsequence will only contain direct calls).

### Complete state / Model termination
After each update the of the modelstate, the model is checked on completion. A model is complete whenever every agent is an expert: every agent knows the secret of every other agent. If this is the case the model will terminate ending the program. However, the user may choose to keep on interacting with the model if they want to.
