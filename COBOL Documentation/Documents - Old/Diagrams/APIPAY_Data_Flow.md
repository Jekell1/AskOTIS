```mermaid
flowchart LR
    subgraph "Input"
        INPUT[Input String<br/>BT-REC Format]
        TRANS_ID[Transaction ID<br/>REPAY-TRANS-ID]
    end
    
    subgraph "APIPAY Program"
        PARSE[Parse Input<br/>Parameters]
        VALIDATE[Validation<br/>Engine]
        ROUTE[Transaction<br/>Router]
        BUFFER[LONPF Buffer<br/>Setup]
        CALL_PROG[Call Payment<br/>Processors]
    end
    
    subgraph "File System"
        BR[Branch File<br/>BR-FILE]
        CD1[Code Definition<br/>CD1-FILE]
        GB[Global Parameters<br/>GB-FILE]
        LN1[Loan Master<br/>LN1-FILE]
        WK[Work File<br/>WK-FILE]
        RC2[Daily Control<br/>RC2-FILE]
        BW1[Batch Work<br/>BW1-FILE]
    end
    
    subgraph "Payment Processors"
        LONPFC[LONPFC<br/>Payment Processing]
        LONPFB[LONPFB<br/>Rebate Processing]
        LONPF9[LONPF9<br/>P&L Processing]
        LONPF7[LONPF7<br/>Repo/Other Processing]
        LONPF2[LONPF2<br/>Reversal/Bankruptcy]
        LONPFA[LONPFA<br/>Payoff Processing]
    end
    
    subgraph "Output"
        OP[Operation File<br/>OP-FILE]
        LOG[Transaction Log<br/>LOG-FILE]
        STATUS[Return Status<br/>0=Success, Other=Error]
    end
    
    INPUT --> PARSE
    TRANS_ID --> PARSE
    PARSE --> VALIDATE
    
    VALIDATE --> BR
    VALIDATE --> CD1
    VALIDATE --> GB
    VALIDATE --> LN1
    VALIDATE --> RC2
    
    BR --> VALIDATE
    CD1 --> VALIDATE
    GB --> VALIDATE
    LN1 --> VALIDATE
    RC2 --> VALIDATE
    
    VALIDATE --> ROUTE
    ROUTE --> BUFFER
    BUFFER --> CALL_PROG
    
    CALL_PROG --> LONPFC
    CALL_PROG --> LONPFB
    CALL_PROG --> LONPF9
    CALL_PROG --> LONPF7
    CALL_PROG --> LONPF2
    CALL_PROG --> LONPFA
    
    LONPFC --> LN1
    LONPFB --> LN1
    LONPF9 --> LN1
    LONPF7 --> LN1
    LONPF2 --> LN1
    LONPFA --> LN1
    
    LONPFC --> WK
    LONPFB --> WK
    LONPF9 --> WK
    LONPF7 --> WK
    LONPF2 --> WK
    LONPFA --> WK
    
    WK --> LONPFC
    WK --> LONPFB
    WK --> LONPF9
    WK --> LONPF7
    WK --> LONPF2
    WK --> LONPFA
    
    LN1 --> LONPFC
    LN1 --> LONPFB
    LN1 --> LONPF9
    LN1 --> LONPF7
    LN1 --> LONPF2
    LN1 --> LONPFA
    
    CALL_PROG --> BW1
    BW1 --> CALL_PROG
    
    LONPFC --> OP
    LONPFB --> OP
    LONPF9 --> OP
    LONPF7 --> OP
    LONPF2 --> OP
    LONPFA --> OP
    
    CALL_PROG --> LOG
    CALL_PROG --> STATUS
    
    style INPUT fill:#e3f2fd
    style APIPAY fill:#f3e5f5
    style "File System" fill:#e8f5e8
    style "Payment Processors" fill:#fff3e0
    style Output fill:#fce4ec
```
