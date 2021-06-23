---
layout: page
title: Introduction
permalink: /theory/introduction
---

This page intends to be a gentle introduction to the material used in this
project. We'll start by explaining the idea first, and then go into some more
detail about the separate parts, taking extra time to show how these parts are
connected. The information presented here is consciously not very formal for
ease of understanding. Where applicable, we will refer to a more technical
explanation elsewhere on this site or sometimes a journal article.

1. Table of contents
{:toc}

## The project

For this project, we wanted to look at the way knowledge plays a role in gossip.
We combine aspects of (dynamic) epistemic logic with (dynamic) gossip. Some of
these subjects have been discussed during the University of Groningen _Logical
Aspects of Multi-Agent Systems_ (LAMAS) AI masters course, but some haven't.
Therefore, we will give a brief introduction to the new parts and talk about the
way these new parts connect with the established knowledge.

## Gossip

The Gossip problem is a formal description of a situation in which gossip
spreads through a network. We describe a group of agents who each have a secret
only they know. They also have the phone numbers of some other agents. When they
call another agent, the agents exchange their phone numbers and all the secrets
they know. These relations are described in a directed graph, where the agents
are the nodes, and the edges represent who knows whose phone number or secret.

The problem sketched above is known as [_dynamic gossip_][dyngossip]. This is
because the agents exchange phone numbers -- leading to a constantly changing
graph of who can call whom. This is interesting, because this allows us to apply
this kind of reasoning in the real world. Some examples of applications of
gossip include the management of distributed databases (e.g. by
[Amazon][aws-gossip]) or creating privacy-friendly social networks such as
[Scuttlebutt][scuttle].

The end goal of a gossip problem is that every agent is an expert, that is,
every agent knows every other agent's secret. This state of the gossip graph is
called a _complete gossip graph_.

### Group calls

The “traditional” definition of (dynamic) gossip assumes calls happen between
two agents, with one agent initiating the call and the other receiving the call.
For this project, we wanted to look at a version of dynamic gossip in which
[group calls][groupcalls] are possible -- i.e., one agent initiates a call to
_multiple_ recipients, and all participants exchange numbers and secrets.

## Epistemic logic

Epistemic logic is used to model agent knowledge in multi-agent systems. We can
use epistemic logic to model gossip. This means we can talk about more than
“Agent A knows the secret of agent B”, for example, we can say “Agent A knows
that agent B knows the secret of C”. This allows us to formulate communication
protocols that use this knowledge to reach a complete gossip graph. We can also
define a more ambitious goal, namely that every agent knows that everyone knows
each other's secrets.

### Knowledge structures

A standard tool for working with epistemic models are Kripke models. These allow
us to intuitively show indistinguishability relations. However, these models get
[very large very quickly][awkward-kripke] in the case of dynamic gossip.
Therefore, we use [_knowledge structures_][k-structs] to represent the gossip
state. These are both smaller and computationally more efficient.

### Knowledge transformers

An important aspect of gossip is updating the gossip graph. Therefore, we also
need a way to update the Kripke model and/or knowledge structure. From Dynamic
Epistemic Logic (DEL) we know that [Action Models][action-models] can be used to
succinctly represent changes to a Kripke model. Their equivalent for knowledge
structures is called a [_knowledge transformer_][k-transformers].

## Putting it all together

### Kripke models for gossip graphs

### Reducing model size

### Transforming knowledge

## References

## Footnotes

[^1]: Note that this requires the assumption that all gossip graphs start from
    an initial model where all agents _only_ know their own secret. In practice,
    it is possible that the initial model _does_ contain secrets. This does not
    matter for our definition: we simply say that this initial model is actually
    a state of another, “true” initial model in which our assumption holds.

[aws-gossip]:      https://status.aws.amazon.com/s3-20080720.html
[scuttle]:         https://scuttlebutt.nz/
[dyngossip]:       {{ site.baseurl }}{% link theory/background.markdown %}#dynamic-gossip
[groupcalls]:      {{ site.baseurl }}{% link theory/this-project.markdown %}
[awkward-kripke]:  {{ site.baseurl }}{% link theory/this-project.markdown %}#why-kripke-models-are-a-bit-awkward
[action-models]:   {{ site.baseurl }}{% link theory/background.markdown %}#action-models
[k-structs]:       {{ site.baseurl }}{% link theory/background.markdown %}#knowledge-structures
[k-transformers]:  {{ site.baseurl }}{% link theory/background.markdown %}#knowledge-structures
[synchronicity]:   {{ site.baseurl }}{% link theory/this-project.markdown %}#synchronicity
[ticks]:           {{ site.baseurl }}{% link theory/this-project.markdown %}#ticks
