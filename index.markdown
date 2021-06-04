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

The model is programmed in Haskell as it is a functional programming language and therefore allows us to define the gossip graph structure and its operators through functions, who will be called approprately. As these functions do not differ much from the formal notation will this not only speed up the conversion from this formal notation into 'functional' code but also allow easier interpretation and explanation of the implementation. Furthermore,  the Action Models from SMCDEL (written in Haskell) can be imported and modified, so that less time is spend on implementing the gossip graphs and more time can be spend implementing the group calls, perchance increasing model performance.

## Model interaction

The user will first have to provide the architecture of the gossip graph. Then, once the graph is initialized, the user will be able to interact with the model through the console, while being presented with a view of the current state of the graph. Once a command is executed, the effects on the state of the gossip graph will be printed. If applicable, possible actions for the agents, as a reaction on the state change of the gossip graph for the respective implemented gossip protocols, are displayed.
