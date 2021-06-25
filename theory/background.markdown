---
layout: page
title: Theoretical Background
permalink: /theory/background
---

1. Table of contents
{:toc}

### Dynamic gossip

Dynamic gossip[^dit18] is a process in which agents
communicate (telephone) “numbers”and“secrets”. This is modelled using a gossip
graph $$G = (A,N,S)$$ with $$A$$ a set of agents and $$N \subseteq S \subseteq
A^2$$ binary relations representing which agents knows whose number and/or
secrets. This process is called _dynamic_ because the graph changes on runtime.
It has been proven[^tij71] that for a gossip graph with
$$n \geq 4$$ agents, the number of calls needed before all agents know all
secrets is $$2n - 4$$.

#### Dynamic Epistemic Gossip

In Dynamic Epistemic Gossip[^her17][^ram21][^dit17], the gossip process is
modelled through the knowledge that agents have at any given point. A
Kripke-like model is established, where every world represents a gossip state,
represented by an initial gossip graph and a sequence of calls that is valid for
this graph. The accessibility relations for any agent are established between
gossip states that are "indistinguishable" for them, given their known secrets
and numbers and their (individual) call history. Through these accessibility
relations, a notion of agent knowledge is made that can be used to create
epistemic protocols: Only allow a call if it satisfies a certain epistemic
formula in the gossip model.

#### Group calls

Lebensold[^leb73] generalised the formula for the number of calls needed in a
gossip graph before every agent knows all secret to say that $$\frac{n - 2}{k -
1} + \frac{n - 1}{k} + 1$$ calls when there is a group of $$n$$ agents with
group calls between $$k$$ agents, and $$1 \leq n \leq k^2$$, and
$$2(\frac{n-2}{k -1})$$ for $$n > k^2$$.

### Gossip protocols

To determine which calls are allowed on a gossip graph, the notion of _Gossip
Protocols_ is used. These exist in both epistemic and "regular" variants. A
protocol is a combination of a protocol condition and a deterministic algorithm:

```
repeat forever
    select agent v ∈ A such that condition φ(u, v) is satisfied 
    execute call uv
```

(Source: Van Ditmarsch (2017)[^dit17])

Currently, the project contains the following
protocols:

- ANY:  $$\varphi(x, y) := \top$$
- Learn New Secrets:  $$\varphi(x, y) := \neg S(x, y)$$
- Possible Information Growth: $$\varphi(x, y) := \hat{K}_x \bigvee_{z \in A} (S(x, z) \leftrightarrow \neg S(y, z)) $$

### Modelling

#### Knowledge Structures

Knowledge Structures[^gat18] are a computationally efficient way of representing
knowledge in a system. It is possible to convert between Kripke models and
Knowledge structures, and they have been shown to be equivalent. A knowledge
structure $$\mathcal{F}$$ is a tuple consisting of a set of atoms $$V$$, a state
law $$\theta$$ and a set of observables $$O_1,\dots,O_n$$ such that
$$\mathcal{F} = (V, \theta, O_1,\dots,O_n)$$.

#### Action Models

Action models[^dit08] are a way to model the way knowledge changes in a model.
They contain information about the way indistinguishability relations change.
Like Kripke models and Knowledge structures, the can also be modelled as a
structure, in this case $$\langle E, \sim, pre \rangle$$ where $$E$$ is a domain
of events, $$\sim$$ is an equivalence relation on $$E$$, and $$pre$$ is a
_precondition function_ that assigns a precondition to each $$e \in E$$.

Action models are used with Kripke models, but an equivalent structure exists
for Knowledge Structures: The Knowledge Transformer[^gat18]. A knowledge
transformer $$\mathcal{X}$$ is a tuple $$(V^+, \theta^+, O_1,\dots,O_n)$$.
Interestingly, a semi-private announcement (i.e., a group call!) can be modelled
quite easily:

$$\mathcal{X} = ((\{p_{\varphi}\}, p_{\varphi} \leftrightarrow \varphi, O_1^+,
\dots, O_n^+))$$

where $$O_i^+ = \{p_{\varphi}\}$$ if $$i \in \Delta$$ and $$O_i^+ = \emptyset$$
otherwise. This shows a semi-private announcement of $$\varphi$$ to a group of
agents $$\Delta$$. (Example taken directly from [^gat18], Ex. 2.5.2)

## References

[^tij71]:
    Tijdeman, R. (1971). On a telephone problem. _Nieuw Archief Voor Wiskunde_,
    _3_(19), 188–192.

[^leb73]:
    Lebensold, K. (1973). Efficient Communication by Phone Calls. _Studies in
    Applied Mathematics_, _52_(4), 345–358. DOI:
    [10/ghrv4s](https://doi.org/10/ghrv4s)

[^dit08]:
    Van Ditmarsch, H., van der Hoek, W., & Kooi, B. (2008). Action Models. In
    _Dynamic epistemic logic_. Springer. DOI: [10/c6zcqh](https://doi.org/c6zcqh)

[^dit17]:
    Van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & 
    Schwarzentruber, F. (2017). Epistemic protocols for dynamic gossip. _Journal
    of Applied Logic_, _20_, 1–31. DOI: [10/f9p6c3](https://doi.org/10/f9p6c3)

[^her17]:
    Herzig, A., & Maffre, F. (2017). How to share knowledge by gossiping. _AI
    Communications_, _30_(1), 1–17. DOI: [10/f94qxh](https://doi.org/10/f94qxh)

[^dit18]:
    Van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & Schwarzentruber,
    F. (2018). Dynamic Gossip. _Bulletin of the Iranian Mathematical Society_,
    _45_(3), 701–728. DOI: [10/cvpm](https://doi.org/10/cvpm)

[^gat18]:
    Gattinger, M. (2018). _New Directions in Model Checking Dynamic Epistemic Logic_
    [PhD Thesis]. Universiteit van Amsterdam. URL: <https://malv.in/phdthesis/>

[^ram21]:
    Ramezanian, R., Ramezanian, R., van Ditmarsch, H., & Gattinger, M. (2021).
    Everyone Knows that Everyone Knows. In M. Mojtahedi, S. Rahman, & M. S. Zarepour
    (Eds.), _Mathematics, Logic, and their Philosophies: Essays in Honour of
    Mohammad Ardeshir_ (pp. 117–133). Springer International Publishing. DOI:
    [10/ggf6](https://doi.org/ggf6)
