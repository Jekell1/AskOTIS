      * 2025.0107 BAH NEW #1696 FOR LOWER CASE LETTER S35Q-155

       01  WS-HEADER-FIELDS.
           03  HEADER1         PIC X(4)  VALUE "date".
           03  HEADER2         PIC X(6)  VALUE "amount".
           03  HEADER3         PIC X(12) VALUE "bank account".
           03  HEADER4         PIC X(11) VALUE "cost center".
           03  HEADER5         PIC X(11) VALUE "ledger acct".
           03  HEADER6         PIC X(10) VALUE "header key".
           03  HEADER7         PIC X(8)  VALUE "line key".
           03  HEADER8         PIC X(15) VALUE "transaction amt".
           03  WS-CR           PIC X     VALUE X"0D".

       01  WS-NET-HEADER-REC.
           03  FILLER          PIC X(28) VALUE
           "JournalKey,JournalLineOrder,".
           03  FILLER          PIC X(23) VALUE
           "CompanyReferenceIDType,".
           03  FILLER          PIC X(39) VALUE
           "CompanyReferenceID,Currency,LedgerType,".
           03  FILLER          PIC X(32) VALUE
           "AccountingDate,JournalEntryMemo,".
           03  FILLER          PIC X(38) VALUE
           "LedgerAccountReferenceID_ParentIDType,".
           03  FILLER          PIC X(34) VALUE
           "LedgerAccountReferenceID_ParentID,".
           03  FILLER          PIC X(29) VALUE
           "LedgerAccountReferenceIDType,".
           03  FILLER          PIC X(38) VALUE
           "ExternalCode_LedgerAccountReferenceID,".
           03  FILLER          PIC X(25) VALUE
           "DebitAmount,CreditAmount,".
           03  FILLER          PIC X(22) VALUE
           "LineCurrency,LineMemo,".
           03  FILLER          PIC X(27) VALUE
           "LineCompanyReferenceIDType,".
           03  FILLER          PIC X(23) VALUE
           "LineCompanyReferenceID,".
           03  FILLER          PIC X(28) VALUE
           "Worktag_Revenue_Category_ID,".
           03  FILLER          PIC X(26) VALUE
           "Worktag_Spend_Category_ID,".
           03  FILLER          PIC X(24) VALUE
           "Worktag_Bank_Account_ID,".
           03  FILLER          PIC X(32) VALUE
           "Worktag_Cost_Center_Reference_ID".
           03  FILLER          PIC X     VALUE X"0D".


       01  STRING-VALUES.
           03  S-1             PIC X(20) VALUE "Company_Reference_ID".
           03  S-2             PIC X(14) VALUE "Account_Set_ID".
           03  S-3             PIC X(17) VALUE "Ledger_Account_ID".
