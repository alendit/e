# Behavioral slicing and the local-maximum failure mode

## Purpose

This note records a development-process failure observed during the Feature 88
and Feature 90 implementation. It describes the problem and the conditions
under which it appeared. It does not select a replacement slicing method or
prescribe a fixed slice size.

The project already expects work packages to be useful stopping points that
move toward the final direction. That expectation is compatible with careful
scoping: a package should still be small enough to understand, review, and
validate. The observed problem is that making slices progressively thinner can
eventually work against both goals.

## Observed failure

Addendum 7 was divided into implementation-layer slices such as pure response
normalization and durable erasure persistence. Each slice had a narrow owner,
focused tests, documentation, and an independent review cycle. Locally, this
made the work easy to describe and allowed reviewers to find real defects.

Globally, however, the intermediate state did not implement the requested
behavior. The core accepted a four-way curation disposition while the adapter
and harness still used the earlier three-way contract. The accepted internal
slices therefore depended on later work to restore an operational end-to-end
path. At that point the working tree contained substantial new implementation
and test machinery, but the model still could not submit an erasure and observe
its effect through the real carrier.

The process continued to optimize and review the internal boundaries even
though those boundaries were not useful stopping points. Progress was real at
the component level, but it was weak evidence of progress toward the complete
implementation goal.

## Why thinner slices can increase local-maximum risk

A thin slice narrows the context needed for implementation and review. That is
valuable while the slice still delivers a coherent improvement. Beyond that
point, the same narrowing can hide the relationships that determine whether the
overall design works.

In particular, an implementation-layer slice can encourage the process to:

- optimize an internal API before its consumer establishes the necessary
  contract;
- prove persistence without proving the behavior that needs persistence;
- defer integration constraints until earlier choices have accumulated tests,
  documentation, and review investment;
- accept a temporarily broken public path across several long-running cycles;
- repeatedly validate and document the same dependency cone without producing
  a newly usable outcome; and
- treat conformance to the proposed slice plan as progress even when the plan
  no longer appears to be the best route to the implementation goal.

These effects increase the probability of settling into a local maximum: each
component becomes increasingly polished relative to its immediate review
target, while changing the boundaries becomes increasingly expensive. The
process can then resist a better global arrangement because too much work has
been organized around the current decomposition.

## Diagnostic signs

The observed run exhibited several signs that the slicing hypothesis needed to
be revisited:

- A completed slice could not be committed or shipped without knowingly
  breaking an existing end-to-end path.
- Its acceptance statement depended on multiple future slices before any new
  externally observable behavior existed.
- Local owner tests passed while integration tests were expected to fail.
- Later slices repeatedly exposed constraints that changed the meaning or
  appropriate boundary of earlier slices.
- Review, documentation, and orchestration time grew faster than the set of
  usable behaviors.
- Several iterations refined internal durability and branch semantics before
  the carrier could express the new operation.
- The process could explain substantial component progress but could not
  demonstrate a correspondingly substantial movement toward the requested
  outcome.

None of these signs proves that a slice is intrinsically wrong. An internal
slice can be an efficient short-lived implementation step. The risk appears
when such a step becomes a full delivery and fixed-point boundary despite not
being independently valuable, or when several dependent internal slices remain
open for a prolonged period.

## Scope remains important

This observation is not an argument for maximizing slice size. Very broad work
packages can obscure ownership, mix unrelated decisions, enlarge review
context, and make failures difficult to localize. Meaningful scoping remains
necessary to make likely changes easy and to keep uncertain decisions local and
reversible.

The tension is therefore not “small slices versus large slices.” It is between
slices selected mainly by implementation layers and slices whose scope remains
meaningful relative to the global implementation goal. A useful decomposition
must preserve enough of the behavior and its consumers to reveal whether the
work is actually moving in the intended direction.

## Iterative planning implication

An initial slice plan is a hypothesis about the cheapest reliable path to the
goal. Evidence gathered during implementation may invalidate that hypothesis.
The iterative process should therefore re-evaluate the proposed decomposition,
not only the code inside it.

When the process observes prolonged non-shippable intermediate states,
repeated downstream contract surprises, disproportionate review overhead, or
weak movement in externally observable behavior, it should explicitly consider
rescoping. Rescoping may change which concerns are reviewed together, combine
dependent work, or establish a different stopping point. The appropriate
change remains a design decision for the work package; this note does not
prescribe one universal response.

## Open questions

- What evidence is sufficient to call a slice a meaningful stopping point when
  some of its value is architectural rather than user-visible?
- How long may an internal, non-shippable slice remain open before the process
  should reconsider the decomposition?
- Which integration failures are acceptable temporary implementation evidence,
  and which indicate that the slice boundary is misplaced?
- When should dependent concerns be reviewed together rather than through
  separate fixed-point cycles?
- How should a process compare the cost of rescoping against the sunk cost of
  the current decomposition without allowing sunk cost to decide the outcome?
- What progress signals best distinguish movement toward the global
  implementation goal from increasingly polished local correctness?
