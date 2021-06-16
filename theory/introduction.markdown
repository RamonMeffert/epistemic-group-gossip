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

### Group calls

The “traditional” definition of (dynamic) gossip assumes calls happen between
two agents, with one agent initiating the call and the other receiving the call.
For this project, we wanted to look at a version of dynamic gossip in which
[group calls][groupcalls] are possible -- i.e., one agent initiates a call to
_multiple_ recipients, and all participants exchange numbers and secrets.

## Epistemic logic



### Knowledge structures

### Knowledge transformers

## Putting it all together

### Kripke models for gossip graphs

### Reducing model size (and improving performance!)

### Transforming knowledge

## References

[aws-gossip]: https://status.aws.amazon.com/s3-20080720.html
[scuttle]: https://scuttlebutt.nz/
[dyngossip]: {{ site.baseurl }}{% link theory/background.markdown %}#dynamic-gossip
[groupcalls]: {{ site.baseurl }}{% link theory/this-project.markdown %}
