---
title: This project
permalink: /theory/this-project/
---

## Dynamic epistemic gossip with group calls

We extend previous definitions of dynamic epistemic gossip (e.g. Herzig &
Maffre, 2017; Ramezanian et al., 2021; van Ditmarsch et al., 2017) with the
notion of group calls. We base our notation off of van Ditmarsch et al. (2017):

> Given a finite set of agents (or nodes) $$A = \{a,b,\dots\}$$, we represent a
> gossip graph $$G$$ with telephone numbers and secrets as a triple $$(A, N, S)$$
> with $$N, S \subseteq A \times A$$. That is, the agents $$A$$ are the vertices and
> $$N,S$$ are binary relations on $$A$$, with $$Nxy$$ (for $$(x,y) \in N$$)
> expressing that $$x$$ knows the (telephone) number of $$y$$, and $$Sxy$$
> expressing that $$x$$ knows the secret of $$y$$.

For the full list of gossip notation used and its definitions, see [here]({{
site.baseurl }}{% link theory/gossip-definitions.markdown %}).

Our main change to this definition concerns the definition of calls and related
definitions. We allow one agent to call some number of other agents. We do not
fix this number, and allow the number of agents to be called to be
protocol-dependent. We define a group call as a call from one agent $$a$$ to
$$k$$ other agents $$b_1, \dots, b_k$$, and write such a call as $$a|b_1 \dots
b_k|$$.

We now consider two possible update mechanics:

### Local broadcast

In a _local broadcast_ scenario, one agents simply announces their knowledge to
all other agents in the group call, and receives their information. However, the
other agents in the group call do not gain more information, except from the
call initiator:

$$
    N^{x|y_1 \dots y_k|}_z =
    \begin{cases}
        N_x \cup (\bigcup_{i = 1}^{k} N_{y_i}) & \text{if} & z = x\\
        N_x \cup N_z & \text{if} & z \in \{ y_1, \dots, y_k \}\\
        N_z & \text{otherwise}
    \end{cases}
    \quad
    (\text{Similar for }S^{x \mid y_1 \dots y_k \mid}_z)
$$

### True group call

In a _true group call_ scenario, there is still one agent initiating the call,
but the call corresponds more closely to real-life group calls; everything
announced in the call is transferred to every agent:

$$
    N^{x|y_1 \dots y_k|}_z =
    \begin{cases}
        N_x \cup (\bigcup_{i = 1}^{k} N_{y_i}) & \text{if} & z \in \{ x, y_1, \dots, y_k \}\\
        N_z & \text{otherwise}
    \end{cases}
    \quad
    (\text{Similar for }S^{x \mid y_1 \dots y_k \mid}_z)
$$

For both of the scenarios above, the call-induced gossip graph is written as $$G^{x \mid y_1 \dots y_k \mid}$$, e.g., when agent $$a$$ calls agents b, c, and d, we write $$G^{a \mid bcd \mid}$$

## Modelling epistemic relations

Instead of the Kripke-like gossip model that is used to model agents' knowledge
about a gossip process such as i.a. (van Ditmarsch et al., 2017), we model this
using Knowledge Structures. As introducted by (Gattinger, 2018), a knowledge
structure is represented by $$\mathcal{F}=(V,\theta,O_a,O_b,\ldots)$$, where
$$V$$ is the vocabulary set of atoms, $$\theta$$ is the state law, a binary
formula that every state (i.e. a valuation of the atoms in $$V$$) needs to
satisfy. The sets $$(O_x)_{x\in A}$$ are the observable atoms of agent $$x$$. An
atom is observable for agent $$x$$ if they are certain of whether this atom is
true or false. 

We define these sets as follows:

\begin{align\*}
    V &= \\{ N(x, y) \mid x, y \in A \\} \cup  
        \\{ S(x, y) \mid x, y \in A \\} \cup  
        \\{ C(x, y) \mid x, y \in A \\},\\\\\
    \theta &= \bigwedge_{a \in A} ( S(a, a) \land N(a, a) ) 
             \land \bigwedge_{x, y \in A} C(x, y) \to ( N(x,y) \land S(x, y) \land S(y, x)),\\\\\
    O_{a \in A} &= \\{ S(a,a) \\} \cup \\{ N(a, b) \mid b \in N_a^{\varepsilon} \\}
\end{align\*}

Where we take our definition of $$C(x,y)$$ from Ramezanian et al. (2021),
replacing the definition given in van Ditmarsch et al. (2017).

### Updating knowledge

We intend to use action models (van Ditmarsch et al., 2008) to update our
knowledge structures when a group call occurs. At this point, we have not yet
defined how this will work. (Note that the library we use for knowledge
structures, `SMCDEL`, allows using action models on knowledge structures.)

## References

van Ditmarsch, H., Kooi, B., & van der Hoek, W. (2008). 6. Action Models. In
Dynamic epistemic logic. Springer.

van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & Schwarzentruber,
F. (2017). Epistemic protocols for dynamic gossip. _Journal of Applied Logic_, _20_,
1–31. DOI: [10/f9p6c3](https://doi.org/10/f9p6c3)

Herzig, A., & Maffre, F. (2017). How to share knowledge by gossiping. _AI
Communications_, _30_(1), 1–17. DOI: [10/f94qxh](https://doi.org/10/f94qxh)

Gattinger, M. (2018). New Directions in Model Checking Dynamic Epistemic Logic
[PhD Thesis]. Universiteit van Amsterdam.

Ramezanian, R., Ramezanian, R., van Ditmarsch, H., & Gattinger, M. (2021).
Everyone Knows that Everyone Knows. In M. Mojtahedi, S. Rahman, & M. S. Zarepour
(Eds.), _Mathematics, Logic, and their Philosophies: Essays in Honour of Mohammad
Ardeshir_ (pp. 117–133). Springer International Publishing.
DOI: [ggf6](https://doi.org/ggf6)
