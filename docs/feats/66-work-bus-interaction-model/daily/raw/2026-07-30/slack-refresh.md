# Slack raw sweep — 2026-07-30, window 11:18:02Z→13:19Z

## Sysco/Indeed concurrency limit (thread p1785401947.167579, #hyper-handle-1000-concurrent-byol-queries C0BLNB6LJQ4)
- 11:19:32 Jonas Eckhardt: go ahead; asked for p50/p90/p99 concurrency across tenants.
- 11:27:25 Jan Böttcher: clarify scope — only Sysco/Indeed, not all tenants.
- 12:05:49/12:07:15/12:07:41 Moritz Kaufmann: agrees scope; keep 44 concurrent-query logic; limiting won't help Sysco/Indeed themselves (remote-bottlenecked); "solve for customer" = large pool + random load balancing.
- 12:08:18/12:08:59 Jonas Eckhardt: monitor once limiting starts; move to lunch.
- 13:08:32 Michael Haubenschild: fewer concurrent slots for Sysco wouldn't worsen throughput (remote-bottlenecked).
- 13:10:21 Moritz Kaufmann: agrees, overstated earlier; combined with queue > slot count, could beat watchdog-kill UX (queued survives until timeout).
- 13:43:46 Jan Böttcher: added action plan to canvas F0BLFBZV9T7.
- Canvas F0BLFBZV9T7 (Slackbot ping at 13:47 CEST/11:47Z noting Jan Böttcher + Alice Rey updates): Action plan now states — Hotfix for 7A: limit Sysco/Indeed tenant concurrency to 100 (queue 1000) to avoid watchdog hangs, using 100 to trigger load balancing without wiring per-tenant queue into routing; for 7B (earliest): roll out concurrency limit for all tenants (queue becomes part of load/routing decision); TBD: increase execution slots (S3). Findings: per tenant&pod last 48h max concurrent 252/288 slots, p99≤199 (5 tenants>100), p90≤113 (15 tenants>50), p50≤33. Sysco/DCF root-cause notes: concurrent Redshift write lock caused ~20min stalls ending in "Missing OID" errors; Sysco sends ~700-1500 queries/min but only ~20 execution slots concurrently used absent lock contention. Three solutions catalogued: S1 per-tenant concurrency limit (chosen path), S2 BYOL/DCF-specific limit, S3 execution-slot increase (impractical at scale, ~30k slots needed).

## Coworker Data 360 un-metering (thread p1785363290.621629, #tmp-search-clipper-scrum C02J1981C9X)
- 11:21:31 (1785403291) Yohan Ginon: confused by "headers not properly sent" framing; asks for concrete un-metered-flow example; code to un-meter exists, DC-GES passes info along but must receive it correctly.
- 12:13:25 Juan Camilo Rodriguez Duran: asks which users/headers are affected.
- 12:37:35 Ghislain Brun: points to Patrick Louie's top message — requirement is to un-meter ALL calls to DC hybrid search for coworker.
- 12:49:03 Yohan Ginon: synced with Juan Camilo Rodriguez Duran + Thomas Foulon; fix options = (a) core app change (patch-deadline risk) or (b) DC-GES (not release-constrained); trying core app first; still unsure about Guillaume Kempf's "did NOT use Agent delegation" post implying other flows need disabling.
- 12:57:25 Ghislain Brun: clarifies — all coworker GES queries to DC need un-metering, not just query suggestions; don't worry about agent delegation for now.
- 12:58:50 Ghislain Brun: would rather fix at GES level (default billing flags false) than core.
- 13:05:15 Yohan Ginon: still needs to segregate Coworker flows from regular Hybrid Search queries that must stay metered.
- No further messages after 13:05:15 CEST in this thread through window end.

## #hyper-agent (C09JJBX40HF), 13:19–14:24 CEST
- 13:19:45 Munendra S N asked Hyper Agent for offcore tenantId/FI-FD of org 00DUF000009Lpf3; answer at 13:23:54: found via tenant_info snapshot (2026-07-30); zero Hyper query activity in cdp_hyperdb_logs_v2 over 7-14d (consistent with DEMO org status); single offcore tenant returned.
- 14:12:25 Vaibhav Jaiswal reported new Q4 regression: Calculated Insights on Query Service (Trino), 1xLarge, daily test; pass (07-28, ~58ms) → latency outlier (07-29, p95 ~87ms vs UB ~68ms, +29%) → hard error-rate FAIL (07-30, 9.59% failure rate, 86/897 requests, threshold 1%). test-id 4a2a0563-370c-49a7-92c2-12652a84c181; tenant "CDP Q4 Query Service - Trino"; release_version 256 unchanged good→bad; FI perf2-uswest2/FD cdp1.
  - Hyper Agent 14:20:59: correlates with W-23609430 (P2, confirmed) — 07-24 migration to new data space driving ~11-14x storage bytes, ~20-65x storage-access count on fewer rows, p95 ~2-3x higher; extra cost is execution/storage not compile/queue. Three changes landed together 07-24 (data space, resource pool database→q4, Hyper build 26022→26029), dominant driver not isolated. Linked JPMC bug W-23629439 (P3) remains suspected test-side only; workload v4→v5 diff unretrievable (HPS artifact API 503/404).
  - Hyper Agent 14:23:42: the 9.59% failures are k6 functional-check failures on HTTP-200 responses at Query-Service/harness layer — no 4xx/5xx, timeouts, or Hyper query errors found for this tenant/scenario (~00:45 UTC 07-30); CI test's Hyper workloads logged 0 errors; a separate grpc-status-13 spike on 07-30 (05:00-10:00 UTC) is unrelated (different tenant/workload); exact failing check assertion/response body/server logs not yet examined.
- 14:14:39 Reghunath P asked Hyper Agent to map a list of Core PODs to FI/FD; Hyper Agent replied it produced the response without tools (flagged "may be speculative or hallucinated") and asked for the actual list/investigation data — no resolution yet.

## Checked, no post-boundary activity
#hyper-alerts, #hyper-engine-triage, #262-hyper-shadowing, #hyper-oncall, #cdp-query-service (retriever-availability thread) — no new messages.
#hyper-release: one unrelated shiproom approval (Per Fuchs, PR #13642, "Enable format json in test1 cdp1 so Transform can test", approved by Kevin Schlieper 15:16 CEST) — not part of tracked threads, informational only.
Keyword sweeps (workspace-wide search, no hits after boundary): W-23555507, #13561, prod26-cdp1, fitRoutingOverloaded, dcqueryv3, DCFresh shadowing, W-23480256.

## Calendar (gog calendar events list --all --from 2026-07-30 --to 2026-07-31 -j)
Two events show `updated` after the 11:18:02Z boundary:
- "266 Query, Storage & Metadata Tech Summit" (dvorona's own calendar, 07-30 09:00-19:30 CEST): updated 2026-07-30T13:16:16.947Z — Google Meet auto-attached 10 recording/chat-transcript files (Recording/Chat 2-5); no time/attendee change.
- "Big Rocks - Release Execution" (organizer sreenivas.shetty; surfaced only via --all sweep of kschlieper@salesforce.com's calendar, dvorona not on attendee list): updated 2026-07-30T12:06:38.942Z, sequence=1 (an edit occurred), start unchanged at 2026-07-30T17:30 America/Los_Angeles. Not dvorona's own event; no prior-version diff available via CLI.
No cancellations, no time changes, no new events found on dvorona's own calendar after the boundary.
