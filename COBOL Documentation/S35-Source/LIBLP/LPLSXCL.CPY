      * COPYMEMBER: LIBLP/LPLSXCL

      ***********************************************************************
      *    LPLSCL    LOAN STATUS REPO INFORMATION CLEAR ROUTINE.
      *
      * REV:
      *  MJD 100995 ADDED LS-X-NET-BAL-DUE LS-X-DEFICIENCY-BAL.
      *  BLV 020719 CLEAR LS-X-CYLINDERS, LS-X-AGENT-PHONE, LS-X-AGENT-FAX
      *  BAH 040913 ADDED CLEAR OF LS-X-TITLE-SENT-DATE
      *  CS  190507 CLEAR FUTURE FIELDS
      * AMM 20210429 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-LSX-FILE SECTION.
           MOVE SPACES TO LSX-REC.

           MOVE 0 TO LSX-BRNO LSX-ACCTNO LSX-SEQNO LSX-LTOUCH-DATE.

           MOVE 0 TO LSX-REPODATE LSX-TITLE-MILEAGE
                        LSX-AUTHORIZED-DATE LSX-ASSIGNED-DATE
                        LSX-REPO-CKNO LSX-REPO-CKDATE LSX-REPO-CKAMT
                        LSX-STORED-CKDATE LSX-STORED-CKNO
                        LSX-STORED-CKAMT
                        LSX-REDEEM-DATE LSX-BUREAU-DATE
                        LSX-10DAY-DATE LSX-10DAY-COST
                        LSX-TITLE-APPL-DATE LSX-TITLE-APPL-COST
                        LSX-COND-MILEAGE LSX-REPAIRS-COST
                        LSX-INS-RB-REQ-DATE LSX-INS-RB-REC-DATE
                        LSX-INS-RB-AMT LSX-RB-REQ-DATE
                        LSX-RB-REC-DATE LSX-RB-AMT
                        LSX-SIC-INS-FILE-DATE LSX-SIC-INS-PAID-DATE
                        LSX-SIC-INS-PAID-AMT LSX-BOOK-VALUE
                        LSX-SOLD-TO-DATE LSX-SOLD-TO-AMT
                        LSX-AUCTION-EXPENSE
                        LSX-DLNO LSX-DLR-RSV-CKNO
                        LSX-DLR-RSV-CKAMT LSX-PL-EXPENSE
                        LSX-NET-BAL-DUE LSX-DEFICIENCY-BAL
              MOVE 0 TO LSX-ASSIGN-COMPLETE-DATE(1)
                        LSX-ASSIGN-CKDATE(1)
                        LSX-ASSIGN-CKNO(1)
                        LSX-ASSIGN-CKAMT(1)
                        LSX-ASSIGN-COMPLETE-DATE(2)
                        LSX-ASSIGN-CKDATE(2)
                        LSX-ASSIGN-CKNO(2)
                        LSX-ASSIGN-CKAMT(2)
              MOVE 0 TO LSX-OTH-EXP-CKDATE(1)
                        LSX-OTH-EXP-CKNO(1)
                        LSX-OTH-EXP-CKAMT(1)
                        LSX-OTH-EXP-CKDATE(2)
                        LSX-OTH-EXP-CKNO(2)
                        LSX-OTH-EXP-CKAMT(2)
                        LSX-OTH-EXP-CKDATE(3)
                        LSX-OTH-EXP-CKNO(3)
                        LSX-OTH-EXP-CKAMT(3)
              MOVE 0 TO LSX-CYLINDERS
                        LSX-AGENT-PHONE
                        LSX-AGENT-FAX
                        LSX-TITLE-SENT-DATE
                        LSX-NEXT-HLD-RVW-DATE LSX-CASE-ID
                        LSX-SUPPLIER-ID LSX-AGENCY-PHONE.

