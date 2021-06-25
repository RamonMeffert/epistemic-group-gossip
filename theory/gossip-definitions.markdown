---
title: Gossip definitions
permalink: /theory/gossip-definitions/
---

A summary of notation and theory presented in _van Ditmarsch et al., 2018_

## Gossip graphs

The capital letters $$P,Q,\dots$$ denote any set of binary relations such that $$P \subseteq A^2$$, where $$A$$ denotes the set of agents $$\{a,b,\dots\}$$. They do not reference a specific set or relation within $$G$$.

| Relation | Symbol | Meaning | Explanation |
| :------- | :-- | :-----------| :-- |
| **Binary relation** | $$Pxy$$ | $$x, y \in A \land (x,y) \in P $$ | Agent $$x$$ has relation $$P$$ to agent $$y$$. |
| **Identity relation** | $$I_A$$ | $$\{\, (x,x) \mid x \in A \,\} $$ | The set of all relations where an agent has a relation with itself. |
| **Converse relation** | $$P^{-1}$$ | $$\{\, (x,y) \mid (y,x) \in P \,\} $$ | The set of relations such that wherever the original set had a relation $$(x,y)$$, the converse relation is $$(y,x)$$ |
| **Composition relation** | $$ P \circ Q$$ | $$\{\, (x, z) \mid \exists y \in A : (x,y) \in P \land (y, z) \in Q \,\}  $$ | The set of all relations where two agents can reach each other by calling one other agent. [^1] |
| _No name_ | $$P_x$$ | $$\{\, y \in A \mid Pxy \,\}$$ | The set of agents that $$x$$ has a connection to. |
| _No name_ | $$P^{i}$$ | $$\begin{cases} P & \text{for}\; i = 1 \\ P^{i-1} \circ P & \text{for}\; i > 1 \end{cases} $$ | All relations in $$P$$ where there are $$i - 1$$ agents between agents. |
| _No name_ | $$ P^*$$ | $$\bigcup_{i \geq 1} N^i $$ | All connections that are possible through (repeated) relation composition in $$P$$. |

### Properties of gossip graphs

| Property | Formal definition | Explanation |
| :-- | :-- | :-- |
| **Weakly connected** | $$ \forall x, y \in A \left(\exists (x, y) \in (P \cup P^{-1}) \right) $$ | For all agents, there exists at least a one-directional connection to all other agents in relation $$P$$ |
| **Strongly connected** | $$  \forall x, y \in A \left((x,y) \in P \right) $$ | Relation $$P$$ holds for every combination of agents |
| **Complete** | $$P = A^2$$ | The relation entails all possible relations |
| **Expert agent** | $$S_x = A$$ | Agent $$x$$ knows all secrets

## Gossip protocols

### Calls and call sequences

A call from agent $$x$$ to agent $$y$$ is written simply as $$xy$$, given that $$x$$ and $$y$$ are both agents in a gossip graph, and that $$x$$ has the number of $$y$$ (i.e. $$Nxy$$)

#### Symbols

Call sequences use lowercase greek letters: $$ \sigma, \pi, \tau, \dots $$

| Symbol | Explanation |
| ------ | ----------- |
| $$ \epsilon $$ | Empty sequence |
| $$ \sigma\, ; \tau $$ | Concatenation |
| $$ \sigma \sqsubseteq \tau $$ | $$\sigma$$ is a prefix of $$\tau$$, i.e. $$\tau = \sigma\mathbin{;}\pi$$ where $$\pi$$ can be $$\epsilon$$  |
| $$ \sigma \sqsubset \tau $$ | $$\sigma$$ is a proper prefix of $$\tau$$, i.e. $$\tau = \sigma\mathbin{;}\pi$$ where $$\pi$$ cannot be $$\epsilon$$ |
| $$ \lvert \sigma \rvert $$ | Length of the call sequence |
| $$ \sigma[i]$$ | The $$i$$th call in the non-empty finite sequence $$\sigma$$ |
| $$ \sigma \vert i $$ | $$ \rho \sqsubseteq \sigma \land \lvert \rho \rvert = i $$ (The first $$i$$ calls in $$\sigma$$) |

#### Definitions

|Symbol | Formal definition| Explanation |
|---|---|---|
| $$xy$$ | — | The call from $$x$$ to $$y$$ |
| $$\overline{xy}$$ | — | The call from $$x$$ to $$y$$ or vice versa |
| $$ \sigma_x$$ | $$\begin{cases} \epsilon & \text{if}\; \sigma = \epsilon\\\tau_x\mathbin{;}uv & \text{if}\; \sigma = \tau\mathbin{;}uv \land (x = u \lor x = v) \\ \tau_x & \text{if}\; \sigma = \tau\mathbin{;}uv \land \lnot(x = u \lor x = v) \end{cases} $$ | The calls in a sequence $$\sigma$$ containing $$x$$ (subsequence) |
| $$P^{xy}$$ | $$ P \, \cup \, (\{ (x,y), (y,x) \} \circ P) $$ | The relation $$P$$ after call $$xy$$ |
| $$G^{xy}$$ | $$(A, N^{xy}, S^{xy})$$ | The state of the gossip graph $$G$$ after call $$xy$$ |
| $$G^{\sigma}$$ | $$\begin{cases} G & \text{if}\; \sigma = \epsilon\\ (G^{xy})^{\tau} & \text{if}\; \sigma = xy\mathbin{;}\tau \end{cases}$$ | The state of the gossip graph $$G$$ after call sequence $$\sigma$$

[^1]: See <https://en.wikipedia.org/wiki/Composition_of_relations> for more details. A useful example can be found under _Definition_. Rewritten in the context of gossip, this becomes: $$ (x,z) \in P \circ Q \iff y \in A : (x,y) \in A \land (y,z) \in A $$, or in words: A relation $$(x,z$$) is in the composed relation $$P \circ Q$$ if and only if there exists another agent in $$A$$ where a relation $$(x,y$$) exists in $$P$$ and a relation $$(y,z$$) exists in $$Q$$.

### Protocol condition

A protocol condition $$\pi(x,y)$$ is a boolean combination of the following consituents.

| Constituent | Intuitive meaning |
|     ---     |        ---        |
| $$S^{\sigma}xy$$          | After a call sequence $$\sigma$$, $$x$$ knows the secret of $$y$$
| $$xy \in \sigma_x$$       | $$x$$ has called $$y$$
| $$yx \in \sigma_x$$       | $$x$$ has been called by $$y$$
| $$\sigma_x = \epsilon$$   | $$x$$ has not called anyone, and has not been called by anyone
| $$\sigma_x = \tau\mathbin{;}xz$$  | The last call $$x$$ made was to $$z$$
| $$\sigma_x = \tau\mathbin{;}zx$$  | The last call to $$x$$ was made by $$z$$

### Gossip protocol

A protocol $$ \mathsf{P} $$ is a combination of a protocol condition $$\pi(x,y)$$ and a non-deterministic algorithm. See page 708 of the paper.

### Permitted call sequence

Not all calls are permitted.

| Call (sequence) | Graph | Permitted iff |
| --------------- | ----- | --------- |
| $$xy$$ | $$G^{\sigma}$$ | $$\sigma$$ possible[^2] on $$G$$; $$x \neq y \; \land \; N^{\sigma}xy \; \land $$ and $$\pi(x,y)$$ holds in $$G^{\sigma}$$
| $$\epsilon$$ | $$G$$ | Always |
| $$\sigma\mathbin{;}xy$$ | $$G$$ | $$xy$$ permitted on $$G^{\sigma}$$, $$\sigma$$ permitted on $$G$$ (recursive definition) |
| $$\lvert \sigma \rvert = \infty$$ | $$G$$ | $$\forall n \in N (\sigma[n+1] \; \text{permitted on} \; G^{\sigma \vert n})$$, i.e. the call after the first $$n$$ calls must be permitted (ad infinitum) |

[^2]: A call sequence is only possible if for all calls $$c_i \in \sigma$$ such that $$\sigma = c_i\mathbin{;}\tau$$, the call is possible, i.e. $$Nc_i$$

### Protocol extension

| Symbol | Meaning |
| ------ | ------- |
| $$\mathsf{P}_G$$ | Extension of protocol $$\mathsf{P}$$ on gossip graph $$G$$. Holds the set of $$\mathsf{P}$$-sequences on $$G$$. |
| $$\mathcal{G}$$ | Collection of gossip graphs |
| $$\mathsf{P}_{\mathcal{G}}$$ | The set of extensions on the set of gossip graphs $$\mathcal{G}$$

### Properties of protocols

| Property | Definition |
| -------- | ---------- |
| **Terminating** | If all call sequences on a graph $$G$$ are terminating, the protocol is also terminating. |
| $$ \mathsf{P}_{\mathcal{G}} \subseteq \mathsf{P}^\prime_{\mathcal{G}}$$ | $$\forall G \in \mathcal{G}(\mathsf{P}_G \subseteq \mathsf{P}^\prime_G)$$ |
| $$ \mathsf{P} \subseteq \mathsf{P}^\prime$$ | $$ \mathsf{P}_{\mathcal{G}} \subseteq \mathsf{P}^\prime_{\mathcal{G}}$$ holds for the initial gossip graphs |

### Properties of call sequences in a protocol

| Property | Meaning |
| -------- | ------- |
| ($$ \mathsf{P} $$)-maximal | A sequence that is $$ \mathsf{P}$$-permitted and no more calls are possible on $$G^{\sigma}$$, or the sequence is infinite. |
| ($$ \mathsf{P} $$)-stuck | $$S^{\sigma}$$ is not complete and $$\sigma$$ is $$\mathsf{P}$$-maximal |
| ($$ \mathsf{P} $$)-fair | $$\sigma$$ is finite, or if it is infinite, $$\sigma$$ is $$\mathsf{P}$$-permitted |
| ($$ \mathsf{P} $$)-maximal for $$B$$ ($$B \subseteq A$$) | All calls between members of $$B$$ are $$ \mathsf{P} $$-maximal |
| ($$ \mathsf{P} $$-)successful | $$\sigma \in \mathsf{P}_G$$ is successful if it is finite and all agents in $$G^{\sigma}$$ are experts |

The $$\mathsf{P}$$- prefix can be ommited if it is clear from context what the protocol is.

### Successfulness

| Assessment | Meaning |
| ---------- | ------- |
| strongly successful | all maximal $$\sigma \in \mathsf{P}_G$$ are successful |
| fairly successful | all maximal fair $$\sigma \in \mathsf{P}_G$$ are successful |
| weakly successful | there is a $$\sigma \in \mathsf{P}_G$$ that is successful |
| unsuccessful | there is **no** $$\sigma \in \mathsf{P}_G$$ that is successful |

The success assessment of $$\mathsf{P}$$ on $$\mathcal{G}$$ is determined by finding which assessment holds for all $$G \in \mathcal{G}$$

### The gossip problem

Given a collection $$\mathcal{G}$$: what is the assessment of $$\mathsf{P}$$ on $$\mathcal{G}$$? i.e. Is $$\mathsf{P}$$ (strongly, fairly, weakly, or un-) successful on $$\mathcal{G}$$?

## References

van Ditmarsch, H., van Eijck, J., Pardo, P., Ramezanian, R., & Schwarzentruber, F. (2018). Dynamic Gossip. _Bulletin of the Iranian Mathematical Society_, 45(3), 701–728. <https://doi.org/10/cvpm>
