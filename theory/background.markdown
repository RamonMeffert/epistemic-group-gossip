---
layout: page
title: Theoretical Background
permalink: /theory/background
---

{% include wip-notice.html %}

## Background

### Dynamic gossip

Dynamic gossip (van Ditmarsch et al., 2018) is a process in which agents
communicate (telephone) “numbers”and“secrets”. This is modelled using a gossip
graph $$G = (A,N,S)$$ with \(A\) a set of agents and $$N \subseteq S \subseteq
A^2$$ binary relations representing which agents knows whose number and/or
secrets. This process is called _dynamic_ because the graph changes on runtime.
It has been proven (by Tijdeman (1971) and others) that for a gossip graph with $$n \geq 4$$ agents, the number
of calls needed before all agents know all secrets is $$2n - 4$$.

#### Dynamic Epistemic Gossip

In Dynamic Epistemic Gossip, the gossip process is modelled through the knowledge that agents have at any given point. A Kripke-like model is established, where every world gepresents a gossip state, represented by an inital gossip graph and a sequence of calls that is valid for this graph. The accissibility relations for any agent are established between gossip states that are ``indistinguishable'' for them, given their known secrets and numbers and their (individual) call history. Through these accessibility relations, a notion of agent knowledge is made that can be used to create epistemic protocols: Only allow a call if it satisfies a certain epistemic formula in the gossip model. 

(Herzig & Maffre, 2017; Ramezanian et al., 2021; van Ditmarsch et al., 2017)

#### Group calls

Lebensold (1973) generalised the formula for the number of calls needed in a
gossip graph before every agent knows all secret to say that $$\frac{n - 2}{k -
1} + \frac{n - 1}{k} + 1$$ calls when there is a group of $$n$$ agents with
group calls between $$k$$ agents, and $$1 \leq n \leq k^2$$, and
$$2(\frac{n-2}{k -1})$$ for $$n > k^2$$.

### Modelling

#### Knowledge Structures

(Gattinger, 2018)

#### Action Models

(Ditmarsch et al., 2008)

## References

Tijdeman, R. (1971). On a telephone problem. _Nieuw Archief Voor Wiskunde_,
_3_(19), 188–192.

Lebensold, K. (1973). Efficient Communication by Phone Calls. _Studies in Applied
Mathematics_, _52_(4), 345–358. DOI: [10/ghrv4s](https://doi.org/10/ghrv4s)

Ditmarsch, H. van, Hoek, W. van der, & Kooi, B. (2008). Action Models. In
_Dynamic epistemic logic_. Springer.

van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & Schwarzentruber,
F. (2017). Epistemic protocols for dynamic gossip. _Journal of Applied Logic_, _20_,
1–31. DOI: [10/f9p6c3](https://doi.org/10/f9p6c3)

Herzig, A., & Maffre, F. (2017). How to share knowledge by gossiping. _AI
Communications_, _30_(1), 1–17. DOI: [10/f94qxh](https://doi.org/10/f94qxh)

van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & Schwarzentruber,
F. (2018). Dynamic Gossip. _Bulletin of the Iranian Mathematical Society_, _45_(3),
701–728. DOI: [10/cvpm](https://doi.org/10/cvpm)

Gattinger, M. (2018). _New Directions in Model Checking Dynamic Epistemic Logic_
[PhD Thesis]. Universiteit van Amsterdam.

Ramezanian, R., Ramezanian, R., van Ditmarsch, H., & Gattinger, M. (2021).
Everyone Knows that Everyone Knows. In M. Mojtahedi, S. Rahman, & M. S. Zarepour
(Eds.), _Mathematics, Logic, and their Philosophies: Essays in Honour of Mohammad
Ardeshir_ (pp. 117–133). Springer International Publishing.
[ggf6](https://doi.org/ggf6)
