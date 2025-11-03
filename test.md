# Executive Briefing: AI-Ready Modernization Strategy

## Key Concerns
- **Superficial modernization risk** – converting COBOL to C# without rethinking architecture may replicate old constraints.  
- **AI unpreparedness** – current APIs are CRUD-focused and lack orchestration or event-driven design.  
- **Domain coupling** – weak boundaries limit evolution toward microservices or agent-driven workflows.  
- **Data accessibility** – memos and rich data aren’t exposed in a way AI can consume (semantic search missing).  
- **Compliance and privacy** – no centralized layer to enforce redaction and consent consistently.  

## Recommendations
- **Adopt a compartmentalized monolith** – strong domain boundaries, future-proof without microservice overhead.  
- **Evolve APIs into domain capabilities** – verbs like *recalculate risk* or *generate contact plan* for AI readiness.  
- **Introduce an event backbone** – publish events (e.g., `Borrower.Updated`, `Payment.Posted`) to trigger agent workflows.  
- **Add a semantic/knowledge sidecar** – vectorize memos and summaries (with PII masking) for natural language queries.  
- **Centralize orchestration** – Orchestrator service that composes APIs, applies policy, and integrates AI agents.  
- **Phase modernization** – start simple, add events + semantic layer, pilot AI use cases, then extract domains if needed.  

---

## Modernization Path (Mermaid)

```mermaid
flowchart LR
    A[Legacy COBOL System] ==> B[Compartmentalized Monolith<br/>(.NET 8, Domain Projects)]
    B ==> C[AI-Ready Platform<br/>(Event Backbone + Semantic Layer + Orchestrator)]
    classDef base fill:#eee,stroke:#555,color:#111
    classDef step fill:#d5e8ff,stroke:#357edd,color:#111
    classDef target fill:#d9f7be,stroke:#52c41a,color:#111
    class A base
    class B step
    class C target

graph TD
  subgraph Orchestration["Agent Orchestrator"]
    O1[Plan tasks & tool selection]
    O2[Compose Domain Capability APIs]
    O3[Policy & PII controls<br/>(masking, consent)]
    O4[Semantic retrieval<br/>(RAG over memos/summaries)]
  end

  subgraph Platform["Event & Semantic Substrate"]
    E[Event Backbone<br/>(e.g., Borrower.Updated,<br/>Payment.Posted)]
    V[Semantic Layer<br/>(Vector/Knowledge store)]
  end

  subgraph DomainAPIs["Domain Capability APIs"]
    D1[Borrower API<br/>• get details<br/>• recalc risk<br/>• prepare contact plan]
    D2[Loan API<br/>• detect risk signals<br/>• compute paydown options]
    D3[Collections API<br/>• prioritize queue]
  end

  subgraph Core["Application / Domain Layers"]
    L[Services, rules, DTO projections]
  end

  DB[(SQL Server DB<br/>DB-first source of truth)]

  %% Wiring
  O1 --> O2
  O2 --> D1
  O2 --> D2
  O2 --> D3
  O3 --> D1
  O3 --> D2
  O3 --> D3
  O4 --> V

  D1 <-->|Queries/Commands| L
  D2 <-->|Queries/Commands| L
  D3 <-->|Queries/Commands| L
  L --> DB

  L -. emits .-> E
  E --> O1
  V --> O4

  classDef orch fill:#d9f7be,stroke:#52c41a,color:#111
  classDef substrate fill:#fff1b8,stroke:#faad14,color:#111
  classDef domain fill:#d5e8ff,stroke:#357edd,color:#111
  classDef core fill:#f5f5f5,stroke:#8c8c8c,color:#111
  class Orchestration orch
  class Platform substrate
  class DomainAPIs domain
  class Core core

sequenceDiagram
  autonumber
  participant Agent as AI Agent
  participant Orch as Orchestrator
  participant Borrower as Borrower API
  participant Loan as Loan API
  participant Sem as Semantic Layer
  participant Events as Event Backbone

  Agent->>Orch: Goal: Reduce delinquency risk for Borrower 123
  Orch->>Borrower: GET /borrowers/123/details (masked)
  Borrower-->>Orch: Borrower details + flags

  Orch->>Loan: POST /loans/9065:detect-risk-signals
  Loan-->>Orch: Risk signals (late pays, fees, trailers)

  Orch->>Sem: Vector search over borrower/loan memos
  Sem-->>Orch: Relevant memos (summaries, no PII)

  Orch->>Borrower: POST /borrowers/123:prepare-contact-plan
  Borrower-->>Orch: Contact plan honoring consent & constraints

  Orch->>Events: Emit Compliance.FlagAdded (ID-only payload)
  Orch-->>Agent: Final plan + rationale + next best actions
